library("psych")

data <- read.csv("raw.csv")

# drop timestamp
data <- data.frame(data[names(data) != "Timestamp"])

columns_map_rev <- c(
  "gender" = "What.is.your.gender..ذكر.ام.انثى.",
  "education" = "What.is.your.current.education.level..المستوى.العلمي.الحالي.",
  "travelling" = "Are.you.planning.to.travel.abroad.within.the.next.two.years...هل.تنوي.السفر.خارج.لبنان.خلال.السنتين.القادمتين.",
  "major" = "What.is.your.area.of.study..ماذا.تدرس.",
  "incomesource" = "What.is.your.main.source.of.income..ما.هو.مصدر.دخلك.",
  "spends" = "How.much.money..in.US.Dollars..do.you.spend.monthly...كم.تصرف.بالشهر...بالدولار.",
  "saves" = "How.much.money..in.US.Dollars..do.you.save.monthly..كم.توفّر.من.المال.جانبا.كل.شهر.",
  "sleeps" = "How.long.do.you.sleep.per.day...كم.ساعة.تنام.باليوم.",
  "socialize" = "How.much.time.do.you.spend.socializing.per.week...كم.ساعة.تقريبا.بالسبوع.تختلط.مع.الناس..اجتماعيات..",
  "news" = "How.much.time.do.you.spend.watching.Lebanese.news...ما.مدى.اهتمامك.بالأخبار.السياسية.و.الاقتصادية..عن.لبنان.",
  "Q1" = "How.often.have.you.been.upset.because.of.something.that.happened.unexpectedly..كم.مرة.انزعجت.بسبب.شيء.حدث.بشكل.غير.متوقع.",
  "Q2" = "How.often.have.you.felt.that.you.were.unable.to.control.important.things.in.your.life...كم.مرة.شعرت.أنك.غير.قادر.على.التحكم.في.الأمور.المهمة.في.حياتك.",
  "Q3" = "How.often.you.felt.nervous.and.stressed...كم.مرة.شعرت.بالتوتر.والضغط.",
  "N1" = "How.often.have.you.dealt.successfully.with.irritating.life.hassles...كم.مرة.تعاملت.بنجاح.مع.متاعب.الحياة.المزعجة.",
  "N2" = "How.often.have.you.felt.that.you.were.effectively.coping.with.important.changes.that.were.occurring.in.your.life...كم.مرة.شعرت.أنك.تتعامل.بفعالية.مع.التغييرات.المهمة.التي.حدثت.في.حياتك.",
  "N3" = "How.often.have.you.felt.confidence.about.your.ability.to.handle.your.personal.problems...كم.مرة.شعرت.بالثقة.بشأن.قدرتك.على.التعامل.مع.مشاكلك.الشخصية.",
  "N4" = "How.often.have.you.felt.that.things.were.going.your.way...كم.مرة.شعرت.أن.الأمور.تسير.في.طريقك.",
  "Q4" = "How.often.have.you.found.that.you.could.not.cope.with.all.the.things.that.you.had.to.do...كم.مرة.وجدت.أنك.لا.تستطيع.التعامل.مع.كل.الأشياء.التي.كان.عليك.القيام.بها.",
  "N5" = "How.often.have.you.been.able.to.control.irritations.in.your.life...كم.مرة.تمكنت.من.السيطرة.على.المضايقات.في.حياتك.",
  "N6" = "How.often.have.you.felt.that.you.were.on.top.of.things...كم.مرة.شعرت.أنك.على.رأس.الأمور.",
  "Q5" = "How.often.have.you.been.angered.because.of.things.that.happened.that.were.outside.of.your.control...كم.مرة.شعرت.بالغضب.بسبب.أشياء.حدثت.خارجة.عن.إرادتك.",
  "Q6" = "How.often.have.you.found.yourself.thinking.about.things.that.you.have.to.accomplish...كم.مرة.وجدت.نفسك.تفكر.في.الأشياء.التي.عليك.إنجازها.",
  "N7" = "How.often.have.you.been.able.to.control.the.way.you.spend.your.time...كم.مرة.تمكنت.من.التحكم.في.الطريقة.التي.تقضي.بها.وقتك.",
  "Q7" = "How.often.have.you.felt.difficulties.were.piling.up.so.high.that.you.could.not.overcome.them...كم.مرة.شعرت.أن.الصعوبات.تتراكم.بشكل.كبير.لدرجة.أنك.لا.تستطيع.التغلب.عليها."
)


# invert dictionary
columns_map <- c()
for (name in names(columns_map_rev)) {
  columns_map[columns_map_rev[name]] <- name
}


# rename columns
for (name in names(data)) {
  names(data)[names(data) == name] <- columns_map[name]
}



# drop major
data <- data.frame(data[names(data) != "major"])


# process continuous variables
dollars_processing <- function(value) {
  if (value == "Around 200/300$") {
    return(250)
  }
  if (value == "100$ to 200$ ") {
    return(150)
  }
  if (value == "٤٠٠$") {
    return(400)
  }
  if (value == "٢٠٠$") {
    return(200)
  }
  if (value == "٥٠$") {
    return(50)
  }
  if (value == "100 to 200") {
    return(150)
  }
  if (value == "٥٠$ تقريباً ") {
    return(50)
  }
  if (value == "10$- 50$") {
    return(30)
  }
  if (value == "200$ to 300$ ") {
    return(250)
  }
  if (value == "مصاريف جامعية فقط أقل من ١٠٠$") {
    return(100)
  }
  if (value == "..") {
    return(70)
  }
  if (value == "مصاريف جامعية فقط أقل من ١٠٠$") {
    return(100)
  }

  value <- gsub("\\D", "", value)
  if (value == "") {
    value <- "0"
  }
  value <- as.numeric(value)
  return(value)
}

data$spends <- as.numeric(lapply(data$spends, dollars_processing))
data$saves <- as.numeric(lapply(data$saves, dollars_processing))

# encode ordinal and nominal variables
gender_encoding <- c("Male" = 0, "Female" = 1)
education_encoding <- c("Bachelors" = 0, "Masters" = 1, "PhD" = 2)
travelling_encoding <- c("No" = 0, "Yes" = 1)
incomesource_encoding <- c("Work" = 0, "Family" = 1, "Loans" = 2)
sleeps_encoding <- c("Between 5 hours and 7 hours" = 0, "More than 7 hours" = 1)
socialize_encoding <- c(
  "Less than 10 hours per week" = 0,
  "Between 10 hours and 20 hours per week" = 1,
  "Between 20 hours and 30 hours per week" = 2,
  "Between 30 hours and 40 hours per week" = 3,
  "More than 40 hours per week" = 4
)
news_encoding <- c(
  "I don't care" = 0,
  "I don't care (لا اهتم)" = 0,
  "Every once in a long while" = 1,
  "Every once in a long while (القليل)" = 1,
  "I just keep myself informed" = 2,
  "I just keep myself informed (فقط لأبقي نفسي على اطلاع)" = 2,
  "A lot" = 3,
  "A lot (كثير)" = 3
)

encode <- function(data, encoding, column) {
  data[[column]] <- as.integer(lapply(data[[column]], function(value) {
    return(encoding[value])
  }))
  return(data)
}


data <- encode(data, gender_encoding, "gender")
data <- encode(data, education_encoding, "education")
data <- encode(data, travelling_encoding, "travelling")
data <- encode(data, incomesource_encoding, "incomesource")
data <- encode(data, sleeps_encoding, "sleeps")
data <- encode(data, socialize_encoding, "socialize")
data <- encode(data, news_encoding, "news")

# check NaNs
if (any(is.na(data))) {
  stop("data contains NA.")
}


write.csv(data, "all_data.csv", row.names = FALSE)
