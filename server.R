library(shiny)
library(ggplot2)

#BAI function
bai = function(height, hip){
  return((100 * hip) / (height * height^(1/2)) - 18)
}

#Women's BMR function
wbmr = function(weight, height, age) {
  return(655 + (4.35 * weight) + (4.7 * height) - (4.7 * age))
}

#Men's BMR function
mbmr = function(weight, height, age) {
  return(66 + (6.23 * weight) + (12.7 * height) - (6.8 * age))
}

#Calories burned from running function
cal_burn = function(weight, miles) {
  return(0.75 * weight * miles)
}

#Weight loss function
weight_loss = function(gender, weight, height, hip, miles, calories, age, counter = 0, weight_vector = c(), overall = 0) {
  counter = counter + 1
  overall = overall_burn(gender, weight, miles, height, age, calories)
  pound_loss_month = overall * 28 / 3500
  while (counter <= 12) {
    return(weight_loss(gender, weight - pound_loss_month, height, hip, miles, calories, age, counter, append(weight_vector, weight - pound_loss_month), overall))
  }
  weight_vector = as.data.frame(weight_vector)
  names(weight_vector) = c("weight")
  weight_vector$month = c(1:12)
  return(weight_vector)
}

#Overall burn
overall_burn = function(gender, weight, miles, height, age, calories) {
  if (gender == "Male"){
    return(cal_burn(weight, miles) / 7 + mbmr(weight, height, age) - calories)
  }
  else {
    return(cal_burn(weight, miles) / 7 + wbmr(weight, height, age) - calories)
  }
}


#Recommendation System
nutr_recommend = function(calories, protein, fat, carbs, sugar, sodium, height, weight, age) {
  ccm = mbmr(weight, height, age) * 1.2
  change = calories - ccm
  sodium_change = sodium - 1.5
  carbs_change = carbs *( 2 / 17)
  sugar_change = sugar * ( 2 / 17 )
  fat_change = fat * ( 9 / 17 )
  recommendations = c()
  protein_need = weight / 2.2 * .9
  p_change = protein - protein_need
  fat_need = ccm * .3 / 9
  f_change = fat - fat_need
  carb_need = ccm * .6 / 9
  c_change = carbs - carb_need
  sugar_need = ccm * .1 / 9
  s_change = sugar - sugar_need
  sodium_need = .35
  sod_change = sodium - sodium_need
  
  
  #calories
  if ( calories > (ccm * 1.1) ) {
    append(recommendations, cat("Decrease your calorie intake by ", change, "to meet your suggested requirement.\n"))
  }
  
  else if ( calories < ( ccm * .9 ) ) {
    append(recommendations, cat("Increase your calorie intake by ", abs(change), "to meet your suggested requirement.\n"))
  }
  else{
    append(recommendations, cat("Your calorie intake is at a good enough level!\n"))
  }
  
  #protein
  if (protein > (protein_need * 1.1)) {
    append(recommendations, cat("Decrease your protein intake by ", p_change, "to meet your suggested requirement.\n"))
  }
  else if (protein < (protein_need * 0.9)) {
    append(recommendations, cat("Increase your protein intake by ", abs(p_change), "to meet your suggested requirement.\n"))
  }
  else {
    append(recommendations, cat("Your protein intake is at a good enough level!\n"))
  }
  
  #fat
  if (fat > (fat_need * 1.1)) {
    append(recommendations, cat("Decrease your fat intake by ", f_change, "to meet your suggested requirement.\n"))
  }
  else if (fat < (fat_need * 0.9)) {
    append(recommendations, cat("Increase your fat intake by ", abs(f_change), "to meet your suggested requirement.\n"))
  }
  else {
    append(recommendations, cat("Your fat intake is at a good enough level!\n"))
  }
  
  #carbs
  if (carbs > (carb_need * 1.1)) {
    append(recommendations, cat("Decrease your carb intake by ", c_change, "to meet your suggested requirement.\n"))
  }
  else if (carbs < (carb_need * 0.9)) {
    append(recommendations, cat("Increase your carb intake by ", abs(c_change), "to meet your suggested requirement.\n"))
  }
  else {
    append(recommendations, cat("Your carb intake is at a good enough level!\n"))
  }
  
  #sugar
  if (sugar > (sugar_need * 1.1)) {
    append(recommendations, cat("Decrease your sugar intake by ", s_change, "to meet your suggested requirement.\n"))
  }
  else if (sugar < (sugar_need * 0.9)) {
    append(recommendations, cat("Increase your sugar intake by ", abs(s_change), "to meet your suggested requirement.\n"))
  }
  else {
    append(recommendations, cat("Your sugar intake is at a good enough level!\n"))
  }
  
  #sodium
  
  return(recommendations)
  
  
}

server = function(input, output) {
  output$user_bmr = renderPrint(cat("You are passively burning", mbmr(input$weight, input$height, input$age), "calories"))
  output$user_bai = renderPrint(cat("Your body fat percentage is currently", bai(input$height * 0.0254, input$waist * 0.0254)))
  output$weight_loss = renderPlot(ggplot(weight_loss(input$gender, input$weight, input$height, input$waist, input$miles, input$calories, input$age), aes(x=month, y=weight), colour = "blue") 
                                  + geom_smooth()
                                  + ggtitle("Weight change in the next 12 months")
                                  + labs(y = "Weight (lbs)", x = "Future Months")
                                  + geom_line(data = weight_loss(input$gender, input$weight, input$height, input$waist, 0, input$calories, input$age), aes(x=month, y=weight), colour = "red")
                                  + scale_colour_manual(values=c("blue", "red")))
  output$recommend = renderPrint(cat(nutr_recommend(input$calories, input$protein, input$fat, input$carbs, input$sugar, input$sodium, input$height, input$weight, input$age)))
  output$weight_table = renderDataTable(weight_loss(input$gender, input$weight, input$height, input$waist, input$miles, input$calories, input$age))
}
