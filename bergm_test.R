# Set up packages
pacman::p_load(igraph,
               tidyverse, 
               Bergm, 
               ergm, 
               data.table, 
               brms)

# Data
warning_tags <- c("No Archive Warnings Apply", "Creator Chose Not To Use Archive Warnings",
                  "Graphic Depictions Of Violence", "Major Character Death",
                  "Rape/Non-Con", "Underage") # read about these tags: https://archiveofourown.org/help/tags_warnings here, 
                                              # These are obligatory, right, so should we exclude them? They feel different.

tags_df <- read_csv("dat/anon_tags.csv") %>%
  mutate(tag = str_remove_all(tags, "'"),
         year = as.numeric(format(date, "%Y"))) %>%
  distinct(anon_work_id, tag, .keep_all = TRUE) %>%
  filter(!tag %in% warning_tags)

# Nodes: try to focus on only the 50 most used tags per fandom:
top_tags <- tags_df %>%
  count(fandom_label, tag) %>%
  group_by(fandom_label) %>%
  slice_max(n, n = 50, with_ties = FALSE) %>%
  summarise(tags = list(tag))
top_tags[[2]]

# Years with at least 100 works
years_ok <- tags_df %>%
  distinct(fandom_label, year, anon_work_id) %>%
  count(fandom_label, year) %>%
  #print(n = 50) %>%
  filter(n >= 100) # we can figure out what the cutoff should be, but put 100 here for now.
years_ok 

#function to make a ties:
make_ties <- function(co, n_works) {
  # see Eck and Waltman for measuring co-occurrence stuff (we want more than chance - decision?)
  expected <- outer(diag(co), diag(co)) / n_works # expected works with both tags if unrelated (chance)
  adj <- (co >= 2 & co > expected) * 1 # defining tie as 2+ works together AND more than expected
  diag(adj) <- 0 # no tag is tied to itself 
  adj # return the 0/1 result
}

# Networks per fandom and year
networks <- tags_df %>%
  semi_join(years_ok, by = c("fandom_label", "year")) %>%
  nest(data = -c(fandom_label, year)) %>%
  left_join(top_tags, by = "fandom_label") %>%
  mutate(n_works = map_int(data, ~ n_distinct(.x$anon_work_id)),
         m       = map2(data, tags, ~ (table(.x$anon_work_id, factor(.x$tag, levels = .y)) > 0) * 1),
         co      = map(m, crossprod),
         adj     = map2(co, n_works, make_ties),
         freq    = map(co, ~ log1p(diag(.x)))) %>%
  arrange(fandom_label, year) %>%
  group_by(fandom_label) %>%
  mutate(prev_adj    = lag(adj),
         consecutive = year - lag(year) == 1) %>%   # only model years right after another year
  ungroup()

networks %>% 
  select(fandom_label, year, n_works, consecutive)

# Network metrics
metrics <- networks %>%
  mutate(density    = map_dbl(adj, ~ mean(.x[upper.tri(.x)])),
         clustering = map_dbl(adj, ~ transitivity(graph_from_adjacency_matrix(.x, mode = "undirected")))) %>%
  select(fandom_label, year, n_works, density, clustering)
metrics

metrics %>%
  pivot_longer(c(density, clustering)) %>%
  ggplot(aes(year, value, colour = fandom_label)) +
  geom_line() + geom_point() +
  facet_wrap(~ name, scales = "free_y") +
  labs(title = "Network metrics per year", x = NULL, y = NULL, colour = NULL) +
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF"))+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(size = 15, color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    strip.background = element_rect(color = "white", fill = "white", linewidth = 1.5, linetype = "solid"),
    strip.text.x = element_text(size = 20, color = "black"),
    strip.text.y = element_text(size = 15, color = "black"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(1, "cm"),
    plot.margin = margin(t = 0.3,  # Top margin
                         r = 0.3,  # Right margin
                         b = 0.3,  # Bottom margin
                         l = 0.3,  # Left margin
                         unit = "cm"))

# Try to plot ties kept, new and dropped
changes <- networks %>%
  filter(consecutive) %>%
  mutate(kept    = map2_dbl(adj, prev_adj, ~ sum(.x == 1 & .y == 1) / 2),
         new     = map2_dbl(adj, prev_adj, ~ sum(.x == 1 & .y == 0) / 2),
         dropped = map2_dbl(adj, prev_adj, ~ sum(.x == 0 & .y == 1) / 2)) %>%
  select(fandom_label, year, kept, new, dropped)
changes

changes %>%
  mutate(dropped = -dropped) %>%
  pivot_longer(c(kept, new, dropped), names_to = "change", values_to = "ties") %>%
  ggplot(aes(factor(year), ties, fill = change)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  facet_wrap(~ fandom_label, ncol = 1) +
  labs(title = "Ties kept, new and dropped", x = NULL, y = "Ties") +
  scale_fill_manual(values = c("#E7298AFF", "#66A61EFF", "#E6AB02FF"))+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(size = 15, color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    strip.background = element_rect(color = "white", fill = "white", linewidth = 1.5, linetype = "solid"),
    strip.text.x = element_text(size = 20, color = "black"),
    strip.text.y = element_text(size = 15, color = "black"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(1, "cm"),
    plot.margin = margin(t = 0.3,  # Top margin
                         r = 0.3,  # Right margin
                         b = 0.3,  # Bottom margin
                         l = 0.3,  # Left margin
                         unit = "cm"))

make_network <- function(adj, freq, prev) { # this year's ties, tag usage, last year's ties
  shared <- prev %*% prev # partners each pair shared last year
  diag(shared) <- 0 # ignore a tag's "shared partners" with itself
  net <- network(adj, directed = FALSE) # make a network object (ties have no direction)
  net %v% "log_freq"    <- freq             # per tag: how often it is used (for nodecov)
  net %n% "prev_tie"    <- prev            # per pair: tied last year? (for edgecov)
  net %n% "prev_shared" <- log1p(shared)   # per pair: log(1 + shared partners last year)
  net  # return the network
}

# Model for one year
# edges                  baseline
# nodecov("log_freq")    control for how often each tag is used
# gwesp                  closure: tags sharing partners are tied
# edgecov("prev_tie")    stability: tied last year -> tied now
# edgecov("prev_shared") delayed closure: shared partners last year -> tied now

ex  <- networks %>% filter(fandom_label == "LOTR RPF", year == 2004)
net <- make_network(ex$adj[[1]], ex$freq[[1]], ex$prev_adj[[1]])

# run ergm first on a single network just for funsies, faster than bergm:
fit_mle <- ergm(net ~ edges + nodecov("log_freq") + gwesp(0.5, fixed = TRUE) +
                  edgecov("prev_tie") + edgecov("prev_shared"))
summary(fit_mle)
mcmc.diagnostics(fit_mle)

# now the real thing: same model, but Bayesian (slower, gives posterior distributions)
# Make this a function, so we can iterate over networks below:
fit_bergm <- function(net) { 
  bergm(net ~ edges + nodecov("log_freq") + gwesp(0.5, fixed = TRUE) +
          edgecov("prev_tie") + edgecov("prev_shared"), # putting these in makes it a discrete-time temporal ERGM.
        prior.mean = rep(0, 5), prior.sigma = diag(5, 5),
        burn.in = 200, main.iters = 2000, aux.iters = 3000)
}

fit_b <- fit_bergm(net)
summary(fit_b)
plot(fit_b)
bgof(fit_b) # models check

# Models for all fandoms and years
terms <- c("edges", "log_freq", "gwesp", "prev_tie", "prev_shared")

# iterate over all of the networks for each fandom and save results:
results <- networks %>%
  filter(consecutive) %>%
  mutate(net = pmap(list(adj, freq, prev_adj), make_network),
         fit = map(net, fit_bergm),
         est = map(fit, ~ tibble(term = terms,
                                 est  = colMeans(.x$Theta),
                                 sd   = apply(.x$Theta, 2, sd)))) %>%
  select(fandom_label, year, n_works, est) %>%
  unnest(est)

write_csv(results, "bergm_results.csv")

# Estimates over time
results %>%
  ggplot(aes(year, est, ymin = est - 2 * sd, ymax = est + 2 * sd, colour = fandom_label)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ term, scales = "free_y") +
  labs(title = "Bergm Estimates Per Year", x = NULL, y = "Posterior Mean (± 2 SD)", colour = NULL) +
  scale_color_manual(values = c("#1B9E77FF", "#D95F02FF", "#7570B3FF"))+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(size = 15, color = "black"),
    legend.title = element_blank(),
    legend.position = "top",
    legend.key.size = unit(1, "lines"),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    strip.background = element_rect(color = "white", fill = "white", linewidth = 1.5, linetype = "solid"),
    strip.text.x = element_text(size = 20, color = "black"),
    strip.text.y = element_text(size = 15, color = "black"),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(1, "cm"),
    plot.margin = margin(t = 0.3,  # Top margin
                         r = 0.3,  # Right margin
                         b = 0.3,  # Bottom margin
                         l = 0.3,  # Left margin
                         unit = "cm"))

# something like this for the meta-analysis:
meta <- brm(est | se(sd) ~ 1 + year_c + log(n_works) + fandom_label,
            data = results %>% filter(term == "prev_shared") %>% mutate(year_c = year - mean(year)))

summary(meta)
pp_check(meta, ndraws = 100)
plot(conditional_effects(meta))

