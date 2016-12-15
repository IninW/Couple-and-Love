# load data
data = read.csv('CoupleAndLove.csv')
library(ggplot2)
library(dplyr)
str(data)
library('gridExtra')

# preparation
colnames(data)
levels(data$OngoingOrNot) = levels(data$OngoingOrNot)[c(2,1)]

# distribution of data
# age
(Age = ggplot(data, aes(x = AgeRatio.M.F.)) +
  geom_histogram(bins = 30))
# 1, then 1.2. 0.5 and 2.5
(Age1 = ggplot(data, aes(x = AgeRatio.M.F., fill = OngoingOrNot)) +
  geom_histogram(bins = 30, position = 'identity', alpha = 0.6))
age = grid.arrange(Age, Age1, nrow = 2, top = 'age_distribution')

age2 = ggplot(data, aes(x = AgeRatio.M.F., y = RelationshipLength)) +
  geom_smooth(method = 'loess')
age3 = ggplot(data, aes(x = AgeRatio.M.F., y = RelationshipLength, color = OngoingOrNot)) +
  geom_smooth(method = 'loess')
Age1 = grid.arrange(age2, age3, nrow = 2, top = 'age-length')

# height
(Height = ggplot(data, aes(x = HeightRatio.M.F.)) +
  geom_histogram(bins = 30))
# 1.13, 1.09, 1.06, 1.02
# largest 1.19
(height1 = ggplot(data, aes(x = HeightRatio.M.F., fill = OngoingOrNot)) +
  geom_histogram(bins = 30, position = 'identity', alpha = 0.6))
# still together, 1.13, 1.19
height = grid.arrange(Height, height1, nrow = 2, top = 'height_distribution')

a = ggplot(data, aes(x = HeightRatio.M.F., y = RelationshipLength)) +
  geom_smooth(method = 'loess')
b = ggplot(data, aes(x = HeightRatio.M.F., y = RelationshipLength, color = OngoingOrNot)) +
  geom_smooth(method = 'loess')
Height1 = grid.arrange(a, b, nrow = 2, top = 'height-length')

# education
(education = ggplot(data, aes(x = EducationGap))+ 
  geom_bar())
(education1 = ggplot(data, aes(x = EducationGap, fill = OngoingOrNot))+ 
  geom_bar(position = position_dodge(0.99)))
# general, 0 gap. Still together, mind 2 here goes up to equal 1
Education = grid.arrange(education, education1, nrow = 2, top = 'education-distri')

data1 = data %>%
  group_by(EducationGap) %>%
  summarize(average_length = mean(RelationshipLength))
a = ggplot(data1, aes(x = EducationGap, y = average_length)) +
  geom_bar(stat = 'identity')
data1 = data %>%
  group_by(EducationGap, OngoingOrNot) %>%
  summarize(average_length = mean(RelationshipLength))
b = ggplot(data1, aes(x = EducationGap, y = average_length, fill = OngoingOrNot)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99))
Education1 = grid.arrange(a, b, nrow = 2, top = 'education-length')  

# zodiac sign
(c = ggplot(data, aes(x = ZodiacCompatibility))+ 
  geom_bar())
(d = ggplot(data, aes(x = ZodiacCompatibility, fill = OngoingOrNot))+ 
  geom_bar(position = position_dodge(0.99)))
# general, 0 gap. Still together, mind 2 here goes up to equal 1
Zodiac = grid.arrange(c, d, nrow = 2, top = 'zodiac-distri')

data1 = data %>%
  group_by(ZodiacCompatibility) %>%
  summarize(average_length = mean(RelationshipLength))
a = ggplot(data1, aes(x = ZodiacCompatibility, y = average_length)) +
  geom_bar(stat = 'identity')
data1 = data %>%
  group_by(ZodiacCompatibility, OngoingOrNot) %>%
  summarize(average_length = mean(RelationshipLength))
b = ggplot(data1, aes(x = ZodiacCompatibility, y = average_length, fill = OngoingOrNot)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99))
Zodiac1 = grid.arrange(a, b, nrow = 2, top = 'zodiac-length') 

# previous relationship number
(c = ggplot(data, aes(x = PreviousRelationship))+ 
  geom_bar())
(d = ggplot(data, aes(x = PreviousRelationship, fill = OngoingOrNot))+ 
  geom_bar(position = position_dodge(0.99)))
# general, 0 gap. Still together, mind 2 here goes up to equal 1
PreviousRelationship = grid.arrange(c, d, nrow = 2, top = 'Previous-distri')

data1 = data %>%
  group_by(PreviousRelationship) %>%
  summarize(average_length = mean(RelationshipLength))
a = ggplot(data1, aes(x = PreviousRelationship, y = average_length)) +
  geom_bar(stat = 'identity')
data1 = data %>%
  group_by(PreviousRelationship, OngoingOrNot) %>%
  summarize(average_length = mean(RelationshipLength))
b = ggplot(data1, aes(x = PreviousRelationship, y = average_length, fill = OngoingOrNot)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99))
PreviousRelationship1 = grid.arrange(a, b, nrow = 2, top = 'previous-length') 

# common interest
(c = ggplot(data, aes(x = CommonInterestDiff.0.3.))+ 
  geom_bar())
(d = ggplot(data, aes(x = CommonInterestDiff.0.3., fill = OngoingOrNot))+ 
  geom_bar(position = position_dodge(0.99)))
# general, 0 gap. Still together, mind 2 here goes up to equal 1
CommonInterestDiff.0.3. = grid.arrange(c, d, nrow = 2, top = 'CommonInterestDiff-distri')

data1 = data %>%
  group_by(CommonInterestDiff.0.3.) %>%
  summarize(average_length = mean(RelationshipLength))
a = ggplot(data1, aes(x = CommonInterestDiff.0.3., y = average_length)) +
  geom_bar(stat = 'identity')
data1 = data %>%
  group_by(CommonInterestDiff.0.3., OngoingOrNot) %>%
  summarize(average_length = mean(RelationshipLength))
b = ggplot(data1, aes(x = CommonInterestDiff.0.3., y = average_length, fill = OngoingOrNot)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99))
CommonInterestDiff.0.3.1 = grid.arrange(a, b, nrow = 2, top = 'CommonInterestDiff-length') 

# family income gap
(c = ggplot(data, aes(x = FamilyIncomeDiff.0.3.))+ 
  geom_bar())
(d = ggplot(data, aes(x = FamilyIncomeDiff.0.3., fill = OngoingOrNot))+ 
  geom_bar(position = position_dodge(0.99)))
# general, 0 gap. Still together, mind 2 here goes up to equal 1
FamilyIncomeDiff.0.3. = grid.arrange(c, d, nrow = 2, top = 'FamilyIncomeDiff-distri')

data1 = data %>%
  group_by(FamilyIncomeDiff.0.3.) %>%
  summarize(average_length = mean(RelationshipLength))
a = ggplot(data1, aes(x = FamilyIncomeDiff.0.3., y = average_length)) +
  geom_bar(stat = 'identity')
data1 = data %>%
  group_by(FamilyIncomeDiff.0.3., OngoingOrNot) %>%
  summarize(average_length = mean(RelationshipLength))
b = ggplot(data1, aes(x = FamilyIncomeDiff.0.3., y = average_length, fill = OngoingOrNot)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99))
FamilyIncomeDiff.0.3.1 = grid.arrange(a, b, nrow = 2, top = 'FamilyIncomeDiff-length') 

# spending pattern difference
(c = ggplot(data, aes(x = SpendingPatternDiff.0.3.))+ 
  geom_bar())
(d = ggplot(data, aes(x = SpendingPatternDiff.0.3., fill = OngoingOrNot))+ 
  geom_bar(position = position_dodge(0.99)))
# general, 0 gap. Still together, mind 2 here goes up to equal 1
SpendingPatternDiff.0.3. = grid.arrange(c, d, nrow = 2, top = 'SpendingPatternDiff-distri')

data1 = data %>%
  group_by(SpendingPatternDiff.0.3.) %>%
  summarize(average_length = mean(RelationshipLength))
a = ggplot(data1, aes(x = SpendingPatternDiff.0.3., y = average_length)) +
  geom_bar(stat = 'identity')
data1 = data %>%
  group_by(SpendingPatternDiff.0.3., OngoingOrNot) %>%
  summarize(average_length = mean(RelationshipLength))
b = ggplot(data1, aes(x = SpendingPatternDiff.0.3., y = average_length, fill = OngoingOrNot)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99))
SpendingPatternDiff.0.3.1 = grid.arrange(a, b, nrow = 2, top = 'SpendingPatternDiff-length') 

# long distance or not
data1 = data%>%
  group_by(LongDistance)%>%
  summarize(average_length = mean(RelationshipLength))
a = ggplot(data1, aes(x = LongDistance, y = average_length, fill = LongDistance)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99)) +
  ggtitle('LongDistance')
data1 = data%>%
  group_by(Home)%>%
  summarize(average_length = mean(RelationshipLength))
b = ggplot(data1, aes(x = Home, y = average_length, fill = Home)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99)) +
  ggtitle('Home')
data1 = data%>%
  group_by(Nationality)%>%
  summarize(average_length = mean(RelationshipLength))
c = ggplot(data1, aes(x = Nationality, y = average_length, fill = Nationality)) +
  geom_bar(stat = 'identity', position = position_dodge(0.99)) +
  ggtitle('Nationality')
Difference = grid.arrange(a, b, c, nrow = 1, top = 'Difference') 
