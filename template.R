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
  protein_change = protein * ( 4 / 17)
  carbs_change = carbs *( 2 / 17)
  sugar_change = sugar * ( 2 / 17 )
  fat_change = fat * ( 9 / 17 )
  
  if ( calories > (ccm * 1.1) ) {
    print("Decrease your calorie intake by ", change)
  }
  
  else if ( calories < ( ccm * .9 ) ) {
    print("Increase your calorie intake by ", abs(change))
  }
  
  else {
    print("Your calorie intake is at the OPTIMAL LEVEL!")
  }
}
