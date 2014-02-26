import java.util.*;

public class minimax_joneshf extends AIModule {

    public int[][] board = new int[7][6];

    public int player;
    public int opponent;
    public int height = 0;
    public int width = 0;
    public TextDisplay td;

    public minimax_joneshf() {
        this.td = new TextDisplay();
        this.player = 0;
        this.opponent = 0;
    }

    public void showBoard(int[][] board) {
        System.out.println(Arrays.deepToString(board));
    }

    public void getNextMove(final GameStateModule game) {
        long start = System.nanoTime();
        int player = game.getActivePlayer();
        this.player = player;
        this.opponent = player == 1 ? 2 : 1;
        this.width = game.getWidth();
        this.height = game.getHeight();
        // Enumerate the board.
        for (int i = 0; i < game.getWidth(); ++i) {
            for (int j = 0; j < game.getHeight(); ++j) {
                this.board[i][j] = game.getAt(i, j);
            }
        }
        System.out.println("that took: "+((System.nanoTime() - start)));
        showBoard(this.board);
        this.td.drawBoard(game);
        int depth = 0;
        while (!this.terminate) {
            minimaxDecision(depth++, this.board, game);
        }
    }

    public void minimaxDecision(int depth, int[][] state, final GameStateModule game) {
        int[] v = maxValue(depth, state, game);
        this.chosenMove = v[1];
    }

    public int[] maxValue(int depth, int[][] state, final GameStateModule game) {

        if (depth == 0 || this.terminate || terminal(state)) {
            return utility(state);
        }

        System.out.println("in maxValue");
        showBoard(state);

        int[] v = {-Integer.MAX_VALUE, 0};
        int temp[];
        for (int[][] s : getMoves(this.player, state, game)) {
            temp = minValue(depth - 1, s, game);
            v = v[0] < temp[0] ? temp : v;
        }

        return v;

    }

    public int[] minValue(int depth, int[][] state, final GameStateModule game) {

        if (depth == 0 || this.terminate || terminal(state)) {
            return utility(state);
        }

        System.out.println("in minValue");
        showBoard(state);

        int[] v = {Integer.MAX_VALUE, 0};
        int temp[];
        for (int[][] s : getMoves(this.opponent, state, game)) {
            temp = maxValue(depth - 1, s, game);
            v = v[0] > temp[0] ? temp : v;
        }

        return v;

    }

    public boolean terminal(int[][] state) {
        long playerBoard = 0x0000000000000000l;
        long opponentBoard = 0x0000000000000000l;
        int pos = 0;
        for (int c = 0; c < this.width; ++c) {
            for (int r = 0; r < this.height; ++r) {
                if (state[c][r] == this.player) {
                    playerBoard |= (c+1) << r;
                } else if (state[c][r] == this.opponent) {
                    opponentBoard |= (c+1) << r;
                }
            }
        }

        System.out.println(playerBoard);
        System.out.println(opponentBoard);

        return false;
    }

    public int[] utility(int[][] state) {
        int[] util = {0, 0};
        return util;
    }

    public int[][][] getMoves(int player, int[][] board, final GameStateModule game) {
        List<int[][]> moves = new ArrayList<int[][]>();
        int height;
        int size = 0;
        for (int i = 0; i < board.length; ++i) {
            if (game.canMakeMove(i)) {
                ++size;
                int[][] newBoard = deepCopy(board);
                height = game.getHeightAt(i);
                newBoard[i][height] = player;
                moves.add(newBoard);
            }
        }
        return moves.toArray(new int[size][][]);
    }

    public int[][] deepCopy(int[][] src) {
        int[][] dest = new int[src.length][];
        for (int i = 0; i < src.length; ++i) {
            dest[i] = Arrays.copyOf(src[i], src[i].length);
        }

        return dest;
    }
}
