library('ggplot2')

load("duin.RData")
load("guo.RData")

duin_summary_q30 <- subset(duin_results$summary, q == 30)
duin_summary_q50 <- subset(duin_results$summary, q == 50)
duin_summary_q100 <- subset(duin_results$summary, q == 100)

guo_summary_q30 <- subset(guo_results$summary, q == 30)
guo_summary_q50 <- subset(guo_results$summary, q == 50)
guo_summary_q100 <- subset(guo_results$summary, q == 100)

p <- ggplot(duin_summary_q50, aes(x = n, y = error, group = method, color = method))
p <- p + geom_path(aes(color = method, linetype = method)) + ylim(c(0, .45))
p <- p + geom_point(aes(shape = method))
p <- p + xlab("Training Data Size") + ylab("Expected Error Rate") + opts(title = plot_title)
p + theme_set(theme_bw())


eer_plots <- function(results, plot_title = "TODO: Add plot title", ymax = 0.5, save = F, file) {
  p <- ggplot(results, aes(x = n, y = error, group = method, color = method))
  p <- p + geom_path(aes(color = method, linetype = method)) + ylim(c(0, ymax))
  p <- p + geom_point(aes(shape = method))
  p <- p + xlab("Training Data Size") + ylab("Expected Error Rate") + opts(title = plot_title)
  p <- p + theme_set(theme_bw())
  if(save) ggsave(filename = file, plot = p)
  p
}

eer_plots(duin_summary_q30, "Duin Simulation Configuration (q = 30)", 0.45)
eer_plots(duin_summary_q50, "Duin Simulation Configuration(q = 50)", 0.45)
eer_plots(duin_summary_q100, "Duin Simulation Configuration (q = 100)", 0.5, save = T, file = "duin100.eps")

eer_plots(guo_summary_q30, "Guo Simulation Configuration (q = 30)", 0.35)
eer_plots(guo_summary_q50, "Guo Simulation Configuration (q = 50)", 0.35)
eer_plots(guo_summary_q100, "Guo Simulation Configuration (q = 100)", 0.40, save = T, file = "guo100.eps")
