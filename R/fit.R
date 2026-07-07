# Chord-constrained text fitting: reflow a sequence of words into a circle so
# that the resulting block of text fits and fills it. Pure arithmetic on word
# widths in millimetres at a reference font size; no grid dependency. The geom
# supplies measured widths and consumes the returned partition and scale.
#
# Geometry:
# - The circle is centred at the origin with effective radius R (the drawn
#   radius minus padding). y increases upward.
# - Each line occupies a rectangle ("band") of height H (font ascent +
#   descent), horizontally centred, with consecutive baselines separated by
#   the leading L (= lineheight * H).
# - The block of k lines is always vertically centred on the circle and every
#   line is nonempty, so the text's optical centre coincides with the circle
#   centre. Line i of k has top B/2 - (i-1)L where B = H + (k-1)L; band
#   positions, and hence the chord width available to each line, are therefore
#   known before any word is assigned.
# - Everything scales linearly with font size, so a layout found at the
#   reference size is applied at scale sigma = R / rho, where rho is the
#   layout's circumradius.
#
# Because every line must be nonempty, feasibility for a fixed line count and
# radius is decided by an interval-reachability dynamic programme (greedy
# first-fit is exact only when it may leave narrow bands empty, which is the
# off-centre freedom the centring constraint excludes). The DP is exact and
# needs no text measurement: line widths come from prefix sums.

# Width of words a..b (inclusive) on one line, from prefix sums
# pre = c(0, cumsum(widths)).
line_width <- function(pre, space, a, b) {
  pre[b + 1L] - pre[a] + (b - a) * space
}

# Precomputed quantities for the reachability DP. A and Ap are adjusted prefix
# sums chosen so that width(words p+1 .. q) <= cap is equivalent to
# A[q] <= cap + Ap[p + 1], which findInterval() answers for all p at once.
flow_context <- function(widths, seg, space) {
  n <- length(widths)
  pre <- c(0, cumsum(widths))
  list(
    n = n,
    pre = pre,
    A = pre[-1] + seq_len(n) * space,
    Ap = pre[seq_len(n)] + seq_len(n) * space,
    seg_end = as.integer(stats::ave(seq_along(seg), seg, FUN = max))
  )
}

# For one band of width cap: e[p + 1] is the furthest word q such that words
# p+1 .. q fit on the band within a single segment (p if not even one fits).
band_reach <- function(ctx, cap) {
  pmin(findInterval(cap + ctx$Ap, ctx$A), ctx$seg_end)
}

# One DP step: given reach[p + 1] = "the first p words are consumable by the
# bands so far", mark everything consumable after one more nonempty band.
flow_step <- function(ctx, reach, e) {
  n <- ctx$n
  p1 <- which(reach[seq_len(n)])
  p1 <- p1[e[p1] >= p1]                    # band must take at least one word
  new <- logical(n + 1L)
  if (length(p1)) {
    d <- tabulate(p1 + 1L, n + 2L) - tabulate(e[p1] + 2L, n + 2L)
    new <- cumsum(d)[seq_len(n + 1L)] > 0L
  }
  new
}

# Can the words be flowed, in order, into exactly length(caps) nonempty lines
# with the given per-line width caps (forced breaks honoured)?
flow_feasible <- function(ctx, caps) {
  reach <- c(TRUE, logical(ctx$n))
  for (cap in caps) {
    reach <- flow_step(ctx, reach, band_reach(ctx, cap))
    if (!any(reach)) return(FALSE)
  }
  reach[ctx$n + 1L]
}

# As flow_feasible, but reconstruct a witness assignment: NULL if infeasible,
# else list(first, last) of word ranges per line. Backtracking prefers the
# largest feasible predecessor, so upper lines are filled as fully as the
# completability of the remainder allows.
flow_witness <- function(ctx, caps) {
  k <- length(caps)
  hist <- vector("list", k)
  ev <- vector("list", k)
  reach <- c(TRUE, logical(ctx$n))
  for (i in seq_len(k)) {
    hist[[i]] <- reach
    ev[[i]] <- band_reach(ctx, caps[i])
    reach <- flow_step(ctx, reach, ev[[i]])
    if (!any(reach)) return(NULL)
  }
  if (!reach[ctx$n + 1L]) return(NULL)
  first <- integer(k)
  last <- integer(k)
  q <- ctx$n
  for (i in k:1) {
    cand <- which(hist[[i]][seq_len(q)])   # predecessors p = cand - 1 < q
    cand <- cand[ev[[i]][cand] >= q]
    p1 <- max(cand)
    first[i] <- p1
    last[i] <- q
    q <- p1 - 1L
  }
  list(first = first, last = last)
}

# Band geometry for a centred block of k lines: top of each line, and the
# vertical extremity (worst corner distance from the horizontal axis).
centred_tops <- function(k, H, L) {
  (H + (k - 1) * L) / 2 - (0:(k - 1)) * L
}

centred_extremities <- function(k, H, L) {
  tops <- centred_tops(k, H, L)
  pmax(abs(tops), abs(tops - H))
}

# Assemble the per-line layout data frame for a centred k-block at radius rho.
centred_layout <- function(ctx, space, H, L, k, rho) {
  ext <- centred_extremities(k, H, L)
  fill <- flow_witness(ctx, 2 * sqrt(pmax(rho^2 - ext^2, 0)))
  if (is.null(fill)) return(NULL)
  lines <- data.frame(
    first = fill$first,
    last = fill$last,
    top = centred_tops(k, H, L)
  )
  lines$bottom <- lines$top - H
  lines$width <- line_width(ctx$pre, space, lines$first, lines$last)
  lines
}

# Minimal feasible circumradius for a centred block of exactly k lines, by
# bisection (feasibility is monotone in rho: wider caps can only extend the
# DP's reach). If rho_beat is finite, only a result better than it matters, so
# one probe at rho_beat prunes the whole bisection when k cannot win.
min_rho_k <- function(ctx, H, L, k, rho_beat = Inf, tol = 1e-7) {
  ext <- centred_extremities(k, H, L)
  feasible <- function(rho) {
    flow_feasible(ctx, 2 * sqrt(pmax(rho^2 - ext^2, 0)))
  }
  lo <- max(ext)                       # top band has zero cap: infeasible
  if (is.finite(rho_beat)) {
    if (rho_beat <= lo || !feasible(rho_beat)) return(Inf)
    hi <- rho_beat
  } else {
    W_all <- ctx$A[ctx$n]
    hi <- sqrt((W_all / 2)^2 + max(ext)^2)  # caps >= whole text: feasible
  }
  while (hi - lo > tol * hi) {
    mid <- (lo + hi) / 2
    if (feasible(mid)) hi <- mid else lo <- mid
  }
  hi
}

# Fit words into the circle at the largest possible font size: the exact
# maximum-scale reflow over centred blocks.
#
# widths  word widths at the reference font size
# seg     segment id per word; a change forces a line break
# space   inter-word space width at the reference font size
# H       line height (ascent + descent) at the reference font size
# L       leading (baseline step), = lineheight * H
# R       effective circle radius (drawn radius minus padding)
# grow    if FALSE, sigma is clamped to 1 (shrink only, never enlarge)
#
# Returns list(sigma, rho, k, lines); lines has one row per typeset line with
# the word range [first, last], line width, and band top/bottom, all at the
# reference size (multiply by sigma to render).
fit_text_in_circle <- function(widths, seg = rep(1L, length(widths)), space,
                               H, L, R, grow = TRUE) {
  stopifnot(length(widths) > 0, all(widths >= 0), space >= 0, H > 0, L > 0,
            R > 0, length(seg) == length(widths), !is.unsorted(seg))
  ctx <- flow_context(widths, seg, space)
  n_seg <- length(unique(seg))

  best_rho <- Inf
  best_k <- NA_integer_
  for (k in n_seg:ctx$n) {
    # A centred k-block reaches (H + (k-1)L)/2 above the centre whatever it
    # holds, so once that alone exceeds the best rho no larger k can win.
    if ((H + (k - 1) * L) / 2 >= best_rho) break
    rho_k <- min_rho_k(ctx, H, L, k, rho_beat = best_rho)
    if (rho_k < best_rho) {
      best_rho <- rho_k
      best_k <- k
    }
  }

  lines <- centred_layout(ctx, space, H, L, best_k, best_rho)
  sigma <- R / best_rho
  if (!grow) sigma <- min(1, sigma)
  list(sigma = sigma, rho = best_rho, k = best_k, lines = lines)
}

# Reflow at natural size: the fewest centred lines that fit inside radius R at
# sigma = 1, shrinking (via the maximum-scale fit) only when no wrapping fits
# unshrunk. This is the reflow behaviour when growth is not requested: text
# wraps to fit but is never enlarged past its set size.
fit_text_natural <- function(widths, seg = rep(1L, length(widths)), space,
                             H, L, R) {
  ctx <- flow_context(widths, seg, space)
  for (k in seq(length(unique(seg)), length(widths))) {
    if (max(centred_extremities(k, H, L)) >= R) break
    lines <- centred_layout(ctx, space, H, L, k, R)
    if (!is.null(lines)) {
      return(list(sigma = 1, rho = circumradius_partition(lines$width, H, L),
                  k = k, lines = lines))
    }
  }
  fit_text_in_circle(widths, seg, space, H, L, R, grow = FALSE)
}

# Circumradius of a fixed partition (the reflow = FALSE path): line widths
# placed as a centred block. Closed form, no search.
circumradius_partition <- function(line_widths, H, L) {
  extremity <- centred_extremities(length(line_widths), H, L)
  max(sqrt((line_widths / 2)^2 + extremity^2))
}

# Choose a layout for one label from its measured word widths and the mode.
# Returns list(sigma, rho, k, lines). With reflow, growth chooses the
# maximum-scale wrapping while its absence wraps at natural size; without
# reflow, the label's own line structure (one line per forced-break segment)
# is kept and only scaled.
fit_label <- function(widths, seg, space, H, L, R, grow, reflow) {
  if (reflow) {
    if (grow) {
      fit_text_in_circle(widths, seg, space, H, L, R, grow = TRUE)
    } else {
      fit_text_natural(widths, seg, space, H, L, R)
    }
  } else {
    seg_ids <- unique(seg)
    pre <- c(0, cumsum(widths))
    first <- vapply(seg_ids, function(s) min(which(seg == s)), integer(1))
    last <- vapply(seg_ids, function(s) max(which(seg == s)), integer(1))
    line_widths <- line_width(pre, space, first, last)
    lines <- data.frame(first = first, last = last,
                        top = centred_tops(length(seg_ids), H, L))
    lines$bottom <- lines$top - H
    lines$width <- line_widths
    rho <- circumradius_partition(line_widths, H, L)
    sigma <- R / rho
    if (!grow) sigma <- min(1, sigma)
    list(sigma = sigma, rho = rho, k = length(seg_ids), lines = lines)
  }
}

# Split a label into words and forced-break segments. Existing line breaks
# ("\n") separate segments; a line is never wrapped across a segment boundary.
tokenise_label <- function(label) {
  label <- gsub("^\\s+|\\s+$", "", label)
  segments <- strsplit(label, "\n", fixed = TRUE)[[1]]
  segments <- segments[nzchar(trimws(segments))]
  words <- lapply(segments, function(s) {
    w <- strsplit(trimws(s), "[^\\S\\r\\n]+", perl = TRUE)[[1]]
    w[nzchar(w)]
  })
  list(
    words = unlist(words),
    seg = rep(seq_along(words), vapply(words, length, integer(1)))
  )
}
