# Predicting a chocolate bar's cocoa bean family
This was an exploratory project to get acquainted with quadratic and linear discriminant analysis as classification algorithms. 
Using this [chocolate ratings dataset](https://www.kaggle.com/datasets/rtatman/chocolate-bar-ratings), I attempted to answer the question:  Given a chocolate's cocoa percentage and expert taste rating, can we tell what type of cocoa bean was used?

The final quadratic discriminant analysis model achieved a moderate accuracy rate of 60%, but still offered valuable insights about the bean families under consideration. The resulting partition matrix showed that data points for Criollo and Forastero were more
spread out, indicating a higher degree of variation in cocoa and rating values. In contrast, Trinitario beans had more consistent cocoa and rating values.

The analysis showed that Forastero beans are used across a wide range of cocoa percentages and receive a broad range of ratings. However, around the average cocoa percentage of 0.7 for all bean families, Trinitario beans dominate. Also in this common midrange for cocoa percentage, Criollo beans receive the highest ratings and are used in chocolates with unusually high cocoa percentages. Given Criollo's rarity, this suggests that it might be used in more niche products.