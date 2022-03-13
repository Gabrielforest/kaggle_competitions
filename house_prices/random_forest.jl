using CSV; using DataFrames; using DecisionTree; using Statistics
# load data
pwd()
readdir()
submission = CSV.read("house_prices/sample_submission.csv", DataFrame)
test = CSV.read("house_prices/test.csv", DataFrame)
train = CSV.read("house_prices/train.csv", DataFrame)

# y_train variable
y_train = train.SalePrice

# removing ID and train.SalePrice
select!(train, Not(:"Id"))
train = select!(train, Not(:"SalePrice"))
test = select!(test, Not(:"Id"))

# concatenating train and test data
all_data = vcat(train, test)

# casting variables
for (i, col) in enumerate(eachcol(all_data))  
    if eltype(col) == String7 
        all_data[!, i] = String.(col)
    elseif eltype(col) == String3 
        all_data[!, i] = String.(col)
    elseif eltype(col) == String1 
        all_data[!, i] = String.(col)
    end 
end

# checking types:
eltype.(eachcol(all_data))

# separate string cols and int cols:
int_cols = select(all_data, findall(col -> eltype(col) <: Int64, eachcol(all_data)))
string_cols = select(all_data, findall(col -> eltype(col) <: String, eachcol(all_data)))

# missing values
for col in eachcol(int_cols)
    replace!(col, missing => mean(col))
end

for col in eachcol(string_cols)
    replace!(col, missing => "missing_value")
end

# col bind:
all_data = hcat(string_cols, int_cols)

# run model
model = RandomForestRegressor(n_trees = 500)

# separating columns in X_train and X_test
X_train = all_data[1:nrow(train), :]
X_test = all_data[nrow(train)+1:end, :]

# conversions to fit the model
y_train = Int.(y_train)
y_train = convert(AbstractVector, y_train)
X_train = Matrix(X_train)

fit!(model, X_train, y_train)

# make predictions
X_test = Matrix(X_test)
y_hat = predict(model, X_test)

#submission file
submission.SalePrice = y_hat
CSV.write("./house_prices/submission_julia.csv", submission)
