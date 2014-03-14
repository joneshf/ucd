%% rank_k_approx: Computes the rank K appoximation of a matrix.
function [result] = rank_k_approx(U, S, V, k)
    %% helper: Helper for this thing
    function [res] = helper(j)
        if j > k
            res = 0;
        else
            sigma_j = S(j, j);
            u_j = U(:, j);
            v_j = V(:, j);
            res = sigma_j * u_j * v_j' + helper(j + 1)
        end
    end
    result = helper(1);
end
