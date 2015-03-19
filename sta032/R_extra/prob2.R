CalcGrade <- function(weights, student) {
  hws         <- subset(student, Category=="HW")$Grade
  exams       <- subset(student, Category=="Exam")$Grade
  finals      <- subset(student, Category=="Final")$Grade

  hw.grade    <- sum(hws) / length(hws) * weights$HW
  exam.grade  <- sum(exams) / length(exams) * weights$Exam
  final.grade <- sum(finals) / length(finals) * weights$Final

  score <- round(sum(hw.grade, exam.grade, final.grade), 2)

  c(score = score, letter = CalcLetter(score))
}

CalcLetter <- function(n) {
  if      (n >= 90) "A"
  else if (n >= 80) "B"
  else if (n >= 70) "C"
  else if (n >= 60) "D"
  else              "F"
}

Min83 <- function(weights, student) {
  cleaned <- na.omit(student)
  possibles <- sapply(c(1:100), function(n) {
    possible <- rbind(cleaned, data.frame(Grade = n, Category = "Final"))
    grade <- CalcGrade(weights, possible)
    if (grade[1] >= 83) n else NA
  })
  min(possibles, na.rm = TRUE)
}
