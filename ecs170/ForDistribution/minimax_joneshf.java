import java.util.*;

public class minimax_joneshf extends AIModule {

    public int[][] board;

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

    public void showBoard(final GameStateModule game) {
        this.td.drawBoard(game);
    }

    public void getNextMove(final GameStateModule game) {
        long start = System.nanoTime();
        int player = game.getActivePlayer();
        this.player = player;
        this.opponent = player == 1 ? 2 : 1;
        this.width = game.getWidth();
        this.height = game.getHeight();
        this.board = new int[this.width][this.height];
        // Enumerate the board.
        for (int i = 0; i < game.getWidth(); ++i) {
            for (int j = 0; j < game.getHeight(); ++j) {
                this.board[i][j] = game.getAt(i, j);
            }
        }
        System.out.println("that took: "+((System.nanoTime() - start)));
        showBoard(game);
        int depth = 0;
        while (!this.terminate && depth < 7) {
            minimaxDecision(depth++, game);
        }
    }

    public void minimaxDecision(int depth, final GameStateModule game) {
        int[] v = maxValue(depth, this.width / 2, game);
        System.out.println("Choosing "+v[1]);
        this.chosenMove = v[1];
    }

    public int[] maxValue(int depth, int col, final GameStateModule game) {

        System.out.println("In maxValue");
        showBoard(game);
        if (depth == 0 || this.terminate || game.isGameOver()) {
            return utility(col, game);
        }

        int[] v = {-Integer.MAX_VALUE, 0};
        int temp[];
        for (int c : getMoves(this.player, game)) {
            game.makeMove(c);
            temp = minValue(depth - 1, c, game);
            v = v[0] < temp[0] ? temp : v;
            game.unMakeMove();
        }

        return v;

    }

    public int[] minValue(int depth, int col, final GameStateModule game) {

        System.out.println("In minValue");
        showBoard(game);
        if (depth == 0 || this.terminate || game.isGameOver()) {
            return utility(col, game);
        }

        int[] v = {Integer.MAX_VALUE, 0};
        int temp[];
        for (int c : getMoves(this.opponent, game)) {
            game.makeMove(c);
            temp = maxValue(depth - 1, c, game);
            v = v[0] > temp[0] ? temp : v;
            game.unMakeMove();
        }

        return v;

    }

    public boolean terminal(final GameStateModule game) {
        long[] boards = gameToBits(game);

        return checkWins(boards[0]) || checkWins(boards[1]);
    }

    public long[] gameToBits(final GameStateModule game) {
        long playerBoard = 0x0000000000000000l;
        long opponentBoard = 0x0000000000000000l;
        int pos = 0;
        for (int c = 0; c < this.width; ++c) {
            for (int r = 0; r < this.height; ++r) {
                if (game.getAt(c, r) == this.player) {
                    playerBoard |= 1 << (c * this.height + r);
                } else if (game.getAt(c, r) == this.opponent) {
                    opponentBoard |= 1 << (c * this.height + r);
                }
            }
        }

        return new long[] {playerBoard, opponentBoard};
    }

    public boolean checkWins(long board) {
        // Diagonal down
        long temp = board & (board >> (this.width - 1));
        if (0 < (temp & (temp >> 2 * (this.width - 1)))) {
            return true;
        }
        // Horizontal
        temp = board & (board >> this.width);
        if (0 < (temp & (temp >> 2 * this.width))) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.width + 1));
        if (0 < (temp & (temp >> 2 * (this.width + 1)))) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 < (temp & (temp >> 2 * 1))) {
            return true;
        }

        return false;
    }

    public boolean checkThrees(long board) {
        // Diagonal down
        long temp = board & (board >> (this.width - 1));
        if (0 < (temp & (temp >> (this.width - 1)))) {
            return true;
        }
        // Horizontal
        temp = board & (board >> this.width);
        if (0 < (temp & (temp >> this.width))) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.width + 1));
        if (0 < (temp & (temp >> (this.width + 1)))) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 < (temp & (temp >> 1))) {
            return true;
        }

        return false;
    }

    public boolean checkTwos(long board) {
        // Diagonal down
        long temp = board & (board >> (this.width - 1));
        if (0 < temp) {
            return true;
        }
        // Horizontal
        temp = board & (board >> this.width);
        if (0 < temp) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.width + 1));
        if (0 < temp) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 < temp) {
            return true;
        }

        return false;
    }

    public int[] utility(int col, final GameStateModule game) {
        long[] boards = gameToBits(game);
        if (checkWins(boards[0])) {
            return new int[] {100, col};
        } else if (checkWins(boards[1])) {
            return new int[] {-100, col};
        } else if (checkThrees(boards[0])) {
            return new int[] {75, col};
        } else if (checkThrees(boards[1])) {
            return new int[] {-75, col};
        } else if (checkTwos(boards[0])) {
            return new int[] {25, col};
        } else if (checkTwos(boards[1])) {
            return new int[] {-25, col};
        } else {
            return new int[] {0, col};
        }
    }

    public List<Integer> getMoves(int player, final GameStateModule game) {
        List<Integer> moves = new ArrayList<Integer>();
        int row;
        int size = 0;
        for (int col = 0; col < this.width; ++col) {
            if (game.canMakeMove(col)) {
                ++size;
                moves.add(col);
            }
        }
        return moves;
    }
}
