addpath(genpath('DeepLearnToolbox'));

[name, mcg, gvh, alm, mit, erl, pox, vac, nuc, category] = textread('yeast.csv', '%s %f %f %f %f %f %f %f %f %s', 'delimiter', ',');

data = horzcat(mcg, gvh, alm, mit, erl, pox, vac, nuc);

cats = cell2mat(arrayfun(@fromCategory, category, 'UniformOutput', false));

numInstances = rows(data);
numTraining = numInstances * 0.75;
randomIndicies = randperm(numInstances);

train_x = data(randomIndicies(1:numTraining), :);
train_y = cats(randomIndicies(1:numTraining), :);

test_x = data(randomIndicies(numTraining + 1:numInstances), :);
test_y = cats(randomIndicies(numTraining + 1:numInstances), :);

rand('state', 0);
nn = nnsetup([8, 3, 10]);
nn.activation_function = 'sigm';
nn.learningRate = 0.05;
nn.momentum = 0;
opts.batchsize = 1;
opts.numepochs = 5;
opts.plot = 1;

[nn, L] = nntrain(nn, train_x, train_y, opts, test_x, test_y);

[er, bad] = nntest(nn, test_x, test_y);

%prob2

opts.numepochs = 20;
opts.plot = 0;

[nn, L] = nntrain(nn, train_x, train_y, opts, test_x, test_y);

sprintf('The training error is %f\n', nn.e)
disp('The activations are'), disp(nn.a{1})

unknown = [0.50, 0.52, 0.52, 0.20, 0.60, 0.00, 0.50, 0.29];
predicted = nnpredict(nn, unknown);
