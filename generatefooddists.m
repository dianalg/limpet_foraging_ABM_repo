% Here we shall generate a large number of food distributions and save
% them, along with an index that states the properties of the
% distributions.  The food patches will be squares in a square grid.
tic
numdists = 1; % Number of food distributions to be created.
N = 500; % Length of the side of the square grid
P = 500; % Number of food patches
mu = 7; % Side length of food patch (area will be mu^2)
sigma = 2; % Variance in side length of food patch
runID = 21; % For use in naming the files

% Create vessels to store the food distributions and the index data
grids = zeros(numdists, N, N);
index = zeros(numdists, 7);

h = waitbar(0,'Initializing waitbar...');

% Call createfooddist to create the food distributions.  Store summary
% statistics in index and the actual distribution in grids.
for(i = 1:numdists)
    [grid, meandist, sddist, nsddist] = newcreatefooddist(N, P, mu, sigma);
    grids(i, :, :) = grid;
    filename = sprintf('runID_%d_dist_%d_N_%d_P_%d_mu_%d_sigma_%0.2f_.csv', runID, i, N, P, mu, sigma);
    csvwrite(filename, grid);
    index(i, :) = [N, P, mu, sigma, meandist, sddist, nsddist];
    %figure; imshow(grid);
    waitbar(i/numdists, h,sprintf('%d maps complete', i))
end
toc
% Stores grids and index in file
filename = sprintf('runID_%d_N_%d_P_%d_mu_%d_sigma_%0.2f_.csv', runID, N, P, mu, sigma);
save('gridtestrun.mat', 'grids', 'index');
% Writes CSV file of index
filename = sprintf('runID_%d_index.csv', runID);
csvwrite(filename, index);
