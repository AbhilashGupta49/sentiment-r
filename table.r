library(RTextTools)

actual = c("negative","negative","neutral","positive","negative","neutral")
predicted= c("negative","neutral","positive","negative","neutral","neutral")

x=table(actual, predicted)
y=recall_accuracy(actual, predicted)



out = file('printed_here.txt', 'w')
capture.output( print(x, print.gap=3), file=out)
write(paste('\n\t\t Accuracy: ',y,sep=''), file=out,append=TRUE)
close(out)
