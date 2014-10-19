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

for i = 2:8
  for j = 2:8
    subplot(7, 7, (j-1)+((i-2) * 7));
    scatter(D(:, i), D(:, j));
  end
end

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
