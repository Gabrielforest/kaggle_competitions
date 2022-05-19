############# Loading packages and data

using Flux, Plots
using Flux: crossentropy, onecold, onehotbatch, train!, params
using LinearAlgebra, Random, Statistics
using CSV, DataFrames

test = CSV.read( "digit_recognizer/test.csv", DataFrame )
train = CSV.read( "digit_recognizer/train.csv", DataFrame )


############# Exploring

# size and head of the data
size( train ), size( test )
first( train, 5 ), first( test, 5 )

# possible digits
unique( train.label )

# view training label image
img = train[ 1, 2:end ] |> Array
# divided by 255 because there are integers in this range: 0 - 255
img = img / 255
img = reshape( img, 28, 28 )
# transpose to see the img vertically
Gray.( img' )

# it supposed to be
train.label[ 1 ]


############# Splitting

# train data
y_train = train.label 
x_train = train[ :, 2:end ] |> Matrix

# function to split randomly by index 
function perclass_splits( y, percent )
    uniq_class = unique( y )
    keep_index = [ ]
    for class in uniq_class
        class_index = findall( y .== class )
        row_index = randsubseq( class_index, percent )
        push!( keep_index, row_index... ) 
    end
    return keep_index
end

# split data between train and test, 75%
train_index = perclass_splits( y_train, 0.75 )
test_index = setdiff( 1:length( y_train ), train_index )

# split features
X_train = x_train[ train_index, : ]
X_test = x_train[ test_index, : ] 

# split classes
Y_train = y_train[ train_index ]
Y_test = y_train[ test_index ]

# one-hot encode labels (ones for position of labels concatenated)
Y_train = onehotbatch( Y_train, 0:9 )
Y_test = onehotbatch( Y_test, 0:9 )


############# Modelling

# define model architecture
model = Chain(
    Dense( 28 * 28, 32, relu ),
    Dense( 32, 10 ),
    softmax
)

# define loss function
loss( x, y ) = crossentropy( model( x ), y )

# track parameters
ps = params( model )

# select optimizer
learning_rate = 0.01
opt = ADAM( learning_rate )

# train model
loss_history = [ ]
epochs = 500

for epoch in 1:epochs
    # train model
    Flux.train!( loss, ps, [ ( X_train', Y_train ) ], opt )
    # print report
    train_loss = loss( X_train', Y_train )
    push!( loss_history, train_loss )
    println( "Epoch = $epoch : Training Loss = $train_loss" )
end

# make predictions
y_hat = model( X_test' )
y = Y_test
# accuracy
mean( y_hat .== y )


############# Results

# display results
check = [ onecold( y_hat )[ i ] == onecold( y )[ i ] for i in 1:length( onecold( y ) ) ]
index = collect( 1:length( onecold( y ) ) )
check_display = [ index onecold( y_hat ) onecold( y ) check ]
check_display = DataFrame( Matrix(check_display),
                          ["index", "predictions", "actual labels", "logical classification"] )
vscodedisplay( check_display )

# number of correct classifications
size( check_display )[ 1 ] - count( x -> x == 0, check_display."logical classification" ) 

# view misclassifications
misclass_index = 9
img = X_test[ misclass_index, : ] / 255 
img = reshape( img, 28, 28 )
Gray.( img )

# labeled as 10
onecold( y )[ misclass_index ]
# classified as 5
onecold( y_hat )[ misclass_index ]

# initialize plot
gr( size = ( 600, 600 ) )

# plot learning curve
p_l_curve = plot( 1:epochs, loss_history,
    xlabel = "Epochs",
    ylabel = "Loss",
    title = "Learning Curve",
    legend = false,
    color = :blue,
    linewidth = 2
)