
US <- covid_effects %>% filter(stateabbrev == "USA")

US %>%
  ggplot(aes(date, students)) +
  geom_line() +
  xlim(as.Date(c('2020-02-28', '2020-05-20'))) +
  labs(title = "Student engagement",
       x = "Date",
       y = "Student engagement Relative to January 2020")
US %>%
  ggplot() +
  geom_line(aes(date, students_q1, color = 'Low-income students')) +
  geom_line(aes(date, students_q4, color = 'High-income students')) +
  xlim(as.Date(c('2020-02-28', '2020-05-20'))) +
  labs(title = "Student engagement for low vs high-income students in Spring 2020 ",
       x = "Date",
       y = "Student engagement Relative to January 2020",
       color = "Place") +
  scale_color_manual(values = c('red', 'blue'))

US %>%
  ggplot() +
  geom_line(aes(date, math_progress_q1, color = 'Math progress of Low-income students')) +
  geom_line(aes(date, math_progress_q4, color = 'Math progress of High-income students')) +
  xlim(as.Date(c('2020-02-28', '2020-05-20'))) +
  labs(title = "Student math progress for low vs high-income students in Spring 2020 ",
       x = "Date",
       y = "Student progress Relative to January 2020",
       color = "Place") +
  scale_color_manual(values = c('red', 'blue'))

MD <- covid_effects %>% filter(stateabbrev == "MD")
  MD %>%
  ggplot() +
  geom_line(aes(date, students, color = 'Maryland')) +
  geom_line(data=US, aes(date, students, color = 'USA')) +
  xlim(as.Date(c('2020-02-28', '2020-05-20'))) +
  labs(title = "Maryland vs. USA Student engagement in Spring 20202",
       x = "Date",
       y = "Student Engagement Levels Relative to January 2020",
       color = "Place") +
  scale_color_manual(values = c('red', 'blue'))

  MD %>%
  ggplot(aes(date, students_q4, color = 'Maryland', linetype = 'High Income')) +
  geom_line() +
  geom_line(aes(date, students_q1, color = 'Maryland', linetype = 'Low Income')) +
  geom_line(data=US, aes(date, emp_q4, color = 'US', linetype = 'High Income')) +
  geom_line(data=US, aes(date, emp_q1, color = 'US', linetype = 'Low Income')) +
  xlim(as.Date(c('2020-02-28', '2020-05-20'))) +
  labs(title = "Student Engagement Across US and Maryland and Income Levels in Spring 2020",
       x = "Date",
       y = "Student engagement Relative to January 2020",
       color = "State",
       linetype = "Income Level") +
  scale_color_manual(values = c('black', 'blue')) +
  scale_linetype_manual(values = c('solid', 'dashed'))
Question 5

US <- covid_effects %>% filter(date == as.Date('2020-04-10'))

US %>%
  ggplot(aes(median_income, math_progress)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = "y ~ x") +
  labs(x = "Median Household Income in 2016", 
       y = "Students' Math Progress",
       title = "Median Household Income in 2016 vs. Students' Math Progress")

  cor_coef <- cor(US$median_income, US$math_progress, use = "complete.obs")
print(cor_coef)

  AR <- school_reopening %>% filter(state_abbrev == "AR")
print(AR)
MD <- school_reopening %>% filter(state_abbrev == "MD")
print(MD)

  MD <- covid_effects %>% filter(stateabbrev == "MD")
AR <- covid_effects %>% filter(stateabbrev == "AR")

  MD %>%
  ggplot() +
  geom_line(aes(date, students, color = 'Maryland')) +
  geom_line(data=AR, aes(date, students, color = 'Arkansas')) +
  xlim(as.Date(c('2020-02-01', '2020-12-25'))) +
  labs(title = "Maryland vs. Arkansas Students' Engagement in all of 2020",
       x = "Date",
       y = "Engagement Rate Relative to January 2020",
       color = "Place") +
  scale_color_manual(values = c('red', 'blue'))

  MD %>%
  ggplot(aes(date, students_q4, color = 'Maryland', linetype = 'High Income')) +
  geom_line() +
  geom_line(aes(date, students_q1, color = 'Maryland', linetype = 'Low Income')) +
  geom_line(data=AR, aes(date, emp_q4, color = 'AR', linetype = 'High Income')) +
  geom_line(data=AR, aes(date, emp_q1, color = 'AR', linetype = 'Low Income')) +
  xlim(as.Date(c('2020-02-01', '2020-12-25'))) +
  labs(title = "Student Engagement in Arkansas vs Maryland across Income Levels in all of 2020",
       x = "Date",
       y = "Student engagement Relative to January 2020",
       color = "State",
       linetype = "Income Level") +
  scale_color_manual(values = c('black', 'blue')) +
  scale_linetype_manual(values = c('solid', 'dashed'))

  AR <- school_reopening %>% filter(state_abbrev == "AR")
print(AR)
MI <- school_reopening %>% filter(state_abbrev == "MI")
print(MI)

  MI <- covid_effects %>% filter(stateabbrev == "MI")
AR <- covid_effects %>% filter(stateabbrev == "AR")

  MI %>%
  ggplot() +
  geom_line(aes(date, students, color = 'Michigan')) +
  geom_line(data=AR, aes(date, students, color = 'Arkansas')) +
  xlim(as.Date(c('2020-09-01', '2020-12-25'))) +
  geom_vline(xintercept = as.Date('2020-11-09')) +
  labs(title = "Michigan vs. Arkansas Students' Engagement in Fall 2020",
       x = "Date",
       y = "Engagement Rate Relative to January 2020",
       color = "Place") +
  scale_color_manual(values = c('red', 'blue'))

  MD <- school_reopening %>% filter(state_abbrev == "MD")
print(MD)
CA <- school_reopening %>% filter(state_abbrev == "CA")
print(CA)

  MD <- covid_effects %>% filter(stateabbrev == "MD")
CA <- covid_effects %>% filter(stateabbrev == "CA")

  MD %>%
  ggplot() +
  geom_line(aes(date, students, color = 'Maryland')) +
  geom_line(data=CA, aes(date, students, color = 'California')) +
  xlim(as.Date(c('2020-02-01', '2020-12-25'))) +
  labs(title = "Maryland vs. California Students' Engagement through 2020",
       x = "Date",
       y = "Engagement Rate Relative to January 2020",
       color = "Place") +
  scale_color_manual(values = c('red', 'blue'))