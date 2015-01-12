%% fromCategory: Go from a category to a 10x1 vector.
function [category] = fromCategory(c)
  if (strcmp(c, 'CYT'))
    category = [1 0 0 0 0 0 0 0 0 0];
  elseif (strcmp(c, 'NUC'))
    category = [0 1 0 0 0 0 0 0 0 0];
  elseif (strcmp(c, 'MIT'))
    category = [0 0 1 0 0 0 0 0 0 0];
  elseif (strcmp(c, 'ME3'))
    category = [0 0 0 1 0 0 0 0 0 0];
  elseif (strcmp(c, 'ME2'))
    category = [0 0 0 0 1 0 0 0 0 0];
  elseif (strcmp(c, 'ME1'))
    category = [0 0 0 0 0 1 0 0 0 0];
  elseif (strcmp(c, 'EXC'))
    category = [0 0 0 0 0 0 1 0 0 0];
  elseif (strcmp(c, 'VAC'))
    category = [0 0 0 0 0 0 0 1 0 0];
  elseif (strcmp(c, 'POX'))
    category = [0 0 0 0 0 0 0 0 1 0];
  elseif (strcmp(c, 'ERL'))
    category = [0 0 0 0 0 0 0 0 0 1];
  endif
end
