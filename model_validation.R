validate <- read.csv("validate.csv")

validate$prob <- predict(final_fit,newdata = validate,type="response")
validate$class <- ifelse(validate$prob>=final_threshold,1,0)

table(cl=validate$class,bn=validate$bankruptcy)
