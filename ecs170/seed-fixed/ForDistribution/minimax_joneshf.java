import java.util.*;

public class minimax_joneshf extends AIModule {

    public class moveComparator implements Comparator<Integer> {
        // compare based on the height.
        // public int compare(Integer i1, Integer i2) {
        //     return Integer.compare(game.getHeightAt(i2), game.getHeightAt(i1));
        // }
        // compare based on the distance from the center.
        public int compare(Integer i1, Integer i2) {
            Integer mid = width / 2;
            return Integer.compare(Math.abs(i1 - mid), Math.abs(i2 - mid));
        }
    }

    public int player;
    public int opponent;
    public int height = 0;
    public int width = 0;
    public boolean firstMove = true;
    public TextDisplay td;

    public int numMax = 0;
    public int numMin = 0;

    public GameStateModule game;

    public minimax_joneshf() {
        this.td = new TextDisplay();
        this.player = 0;
        this.opponent = 0;
    }

    public void showBoard() {
        this.td.drawBoard(this.game);
    }

    public List<Integer> getMoves() {
        List<Integer> moves = new ArrayList<Integer>();
        int row;
        int size = 0;
        for (int col = 0; col < this.game.getWidth(); ++col) {
            if (this.game.canMakeMove(col)) {
                ++size;
                moves.add(col);
            }
        }
        // Collections.sort(moves, new moveComparator());
        return moves;
    }

    public void getNextMove(final GameStateModule game) {
        int player = game.getActivePlayer();
        this.game = game;
        this.player = player;
        this.opponent = player == 1 ? 2 : 1;
        this.width = this.game.getWidth();
        this.height = this.game.getHeight();
        if (this.firstMove && this.player == 1) {
            this.chosenMove = this.width / 2;
            this.firstMove = false;
            return;
        }
        int maxDepth = this.width;
        for (int i = 0; i < this.width; ++i) {
            maxDepth *= Math.max(this.height - this.game.getHeightAt(i), 1);
        }
        int depth = maxDepth - 1;
        while (!this.terminate && depth <= maxDepth) {
            minimaxDecision(depth++);
        }
    }

    public void minimaxDecision(int depth) {
        this.chosenMove = maxValue(depth, 0)[1];
    }

    public int[] maxValue(int depth, int col) {
        if (this.terminate || this.game.isGameOver()/* || depth == 0*/) {
            return new int[] {utility(col), 0};
        }

        int max = -Integer.MAX_VALUE;
        int nextValue;
        int nextMove = 0;
        int[] ret = {0, 0};
        int[] tempRet = {0, 0};
        for (int c : getMoves()) {
            this.game.makeMove(c);
            tempRet = minValue(depth - 1, c);
            if (tempRet[0] > max) {
                max = tempRet[0];
                ret[0] = tempRet[0];
                ret[1] = c;
            }
            this.game.unMakeMove();
        }
        return ret;
    }

    public int[] minValue(int depth, int col) {
        if (this.terminate || this.game.isGameOver()/* || depth == 0*/) {
            return new int[] {utility(col), 0};
        }

        int min = Integer.MAX_VALUE;
        int nextValue;
        int nextMove = 0;
        int[] ret = {0, 0};
        int[] tempRet = {0, 0};
        for (int c : getMoves()) {
            this.game.makeMove(c);
            tempRet = maxValue(depth - 1, c);
            if (tempRet[0] < min) {
                min = tempRet[0];
                ret[0] = tempRet[0];
                ret[1] = c;
            }
            this.game.unMakeMove();
        }
        return ret;
    }

    public long[] gameToBits() {
        long playerBoard = 0l;
        long opponentBoard = 0l;
        int pos = 0;
        for (int c = 0; c < this.game.getWidth(); ++c) {
            for (int r = 0; r < this.game.getHeight(); ++r) {
                if (this.game.getAt(c, r) == this.player) {
                    playerBoard |= 1l << (c * (this.game.getHeight() + 1) + r);
                } else if (this.game.getAt(c, r) == this.opponent) {
                    opponentBoard |= 1l << (c * (this.game.getHeight() + 1) + r);
                }
            }
        }

        return new long[] {playerBoard, opponentBoard};
    }

    public boolean checkWins(long board) {
        // Diagonal down
        long temp = board & (board >> (this.height - 2));
        if (0 != (temp & (temp >> 2 * (this.height - 2)))) {
            return true;
        }
        // Horizontal
        temp = board & (board >> (this.height + 1));
        if (0 != (temp & (temp >> 2 * (this.height + 1)))) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.height + 2));
        if (0 != (temp & (temp >> 2 * (this.height + 2)))) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 != (temp & (temp >> 2 * 1))) {
            return true;
        }

        return false;
    }

    public boolean checkThrees(long board) {
        // Diagonal down
        long temp = board & (board >> (this.height - 2));
        if (0 != (temp & (temp >> (this.height - 2)))) {
            return true;
        }
        // Horizontal
        temp = board & (board >> (this.height + 1));
        if (0 != (temp & (temp >> (this.height + 1)))) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.height + 2));
        if (0 != (temp & (temp >> (this.height + 2)))) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 != (temp & (temp >> 1))) {
            return true;
        }

        return false;
    }

    public boolean checkTwos(long board) {
        // Diagonal down
        long temp = board & (board >> (this.height - 2));
        if (0 != temp) {
            return true;
        }
        // Horizontal
        temp = board & (board >> (this.height + 1));
        if (0 != temp) {
            return true;
        }
        // Diagonal up
        temp = board & (board >> (this.height + 2));
        if (0 != temp) {
            return true;
        }
        // Vertical
        temp = board & (board >> 1);
        if (0 != temp) {
            return true;
        }

        return false;
    }

    public boolean checkBlocks(long player, long opponent) {
        // We need to see if 3 in a row for the opponent
        // intersects with one of the player's pieces.
        // Make the diagonal for opponent.

        // Backslash up
        long oppo = opponent & (opponent >> (this.height - 2));
        oppo &= oppo >> (this.height - 2);
        boolean block = 0 != (player & (oppo >> (this.height - 2)));

        // Backslash down
        oppo = opponent & (opponent << (this.height - 2));
        oppo &= (oppo << (this.height - 2));
        block |= 0 != (player & (oppo << (this.height - 2)));

        // Forwardslash up
        oppo = opponent & (opponent << (this.height + 2));
        oppo &= oppo << (this.height + 2);
        block |= 0 != (player & (oppo << (this.height + 2)));

        // Forwardslash down
        oppo = opponent & (opponent >> (this.height + 2));
        oppo &= (oppo >> (this.height + 2));
        block |= 0 != (player & (oppo >> (this.height + 2)));

        // Horizontal right
        oppo = opponent & (opponent << (this.height + 1));
        oppo &= oppo << (this.height + 1);
        block |= 0 != (player & (oppo << (this.height + 1)));

        // Horizontal left
        oppo = opponent & (opponent >> (this.height + 1));
        oppo &= (oppo >> (this.height + 1));
        block |= 0 != (player & (oppo >> (this.height + 1)));

        // Vertical up
        oppo = opponent & (opponent << 1);
        oppo &= oppo << 1;
        block |= 0 != (player & (oppo << 1));

        // Vertical down
        oppo = opponent & (opponent >> 1);
        oppo &= (oppo >> 1);
        block |= 0 != (player & (oppo >> 1));

        return block;
    }

    public int utility(int col) {
        long[] boards = gameToBits();
        int total = 0;
        int row = this.game.getHeightAt(col);
        long player = 1 << (col * (this.height + 1) + row);
        if (this.game.isGameOver()) {
            switch (this.game.getWinner()) {
                case 1:
                    return Integer.MAX_VALUE;
                case 2:
                    return -Integer.MAX_VALUE;
                default:
                    return 0;
                }
        }
        if (this.game.getActivePlayer() == 1) {
            if (checkBlocks(player, boards[1])) {
                return 500;
            }
            if (checkBlocks(player, boards[0])) {
                return 1500;
            }
        } else if (this.game.getActivePlayer() == 2) {
            if (checkBlocks(player, boards[0])) {
                return -500;
            }
            if (checkBlocks(player, boards[1])) {
                return -1500;
            }
        }
        // if (checkWins(boards[0])) {
        //     return 250;
        // }
        // if (checkWins(boards[1])) {
        //     return -250;
        // }
        // if (checkBlocks(player, boards[1])) {
        //     return 500;
        // }
        // if (checkBlocks(player, boards[0])) {
        //     return -500;
        // }
        if (checkThrees(boards[0])) {
            return 75;
        }
        if (checkThrees(boards[1])) {
            return -75;
        }
        if (checkTwos(boards[0])) {
            return 25;
        }
        if (checkTwos(boards[1])) {
            return -25;
        }
        return total;
    }
}
