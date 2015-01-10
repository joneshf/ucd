> fallGPA <- subset(data, Semester=="Fall", select=GPA)$GPA
> springGPA <- subset(data, Semester=="Spring", select=GPA)$GPA
> png(filename="R_hw1/f.png")
> boxplot(fallGPA, springGPA, main="GPA for Intro. Stats. at Wash. Uni.",
      names=c("Fall", "Spring"), ylab="GPA")
> dev.off()
