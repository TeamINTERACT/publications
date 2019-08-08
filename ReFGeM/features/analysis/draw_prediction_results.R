pred_data <- read.csv("prediction_results_new.csv")

breaks <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)


png("prediciton_results_new.png", width =5, height = 4, units = 'in', res = 300)
# pdf("prediciton_results.pdf", width = 5, height = 4, units = 'in', res=300)
ggplot(pred_data, aes(x=pred_data$features, y=pred_data$mean)) +
  geom_errorbar(aes(ymin=mean-std, ymax=mean+std), width=0.1) +
  geom_line() +
  geom_point() +
  scale_x_continuous(name="Number of selected features", breaks=pred_data$features) +
  xlab("Number of selected features") +
  ylab("Prediction results") +
  theme_bw() +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=12))
dev.off()