# Train Test Split
X_train, X_test, y_train, y_test = train_test_split(X_encoded,y,random_state = 24)

# # Regression approach
reg = LinearRegression().fit(X_train, y_train)
print(reg.score(X_train, y_train))
print(reg.score(X_test, y_test))
