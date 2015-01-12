%% toCategory: Go from a 10x1 vector to a category.
function [category] = toCategory(c)
  if (c == [1 0 0 0 0 0 0 0 0 0])
    category = 'CYT';
  elseif (c == [0 1 0 0 0 0 0 0 0 0])
    category = 'NUC';
  elseif (c == [0 0 1 0 0 0 0 0 0 0])
    category = 'MIT';
  elseif (c == [0 0 0 1 0 0 0 0 0 0])
    category = 'ME3';
  elseif (c == [0 0 0 0 1 0 0 0 0 0])
    category = 'ME2';
  elseif (c == [0 0 0 0 0 1 0 0 0 0])
    category = 'ME1';
  elseif (c == [0 0 0 0 0 0 1 0 0 0])
    category = 'EXC';
  elseif (c == [0 0 0 0 0 0 0 1 0 0])
    category = 'VAC';
  elseif (c == [0 0 0 0 0 0 0 0 1 0])
    category = 'POX';
  elseif (c == [0 0 0 0 0 0 0 0 0 1])
    category = 'ERL';
  end
end
