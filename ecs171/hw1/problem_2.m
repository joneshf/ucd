D = load('auto-mpg-nameless.data');
mpg = D(:, 1);
cyl = D(:, 2);
dis = D(:, 3);
horse = D(:, 4);
weight = D(:, 5);
accel = D(:, 6);
year = D(:, 7);
origin = D(:, 8);

figure();

%% plotall: Creates 49 scatter plots
function plotall(D, n)
  for i = 1:n
    subplot(n-1, n-1, i);
    scatter(D(:, i+1), D(:, i+1));
  end
endfunction

plotall(D, 4);

% for dim=1:4
%   subplot(2,2,dim)
%   if dim<=2
%     boxplot(X(:,dim), y, 'notch', 'off');
%   else
%     boxplot(X(:,dim), y, 'notch', 'on');
%   end
%   set(gca,'xticklabel',classnames);
%   xlabel(''); ylabel('');
%   title(sprintf('%s', varnames{dim}))
% end
