import java.util.*;

public class minimax_joneshf extends AIModule {

    public GameStateModule game;
    public GameStateModule staticGame;
    public boolean useStatic;
    public TextDisplay td = new TextDisplay();
    public int width;
    public int player;
    public int opponent;

    public void getNextMove(final GameStateModule game) {
        this.game = game;
        this.width = this.game.getWidth();
        this.player = this.game.getActivePlayer();
        this.opponent = this.player == 1 ? 2 : 1;
        int depth = 0;
        while (!this.terminate) {
            minimaxDecision(++depth, game);
        }
    }

    public List<Integer> availableMoves(GameStateModule game) {
        List<Integer> moves = new ArrayList<Integer>();
        for (int c = 0; c < this.width; ++c) {
            if (game.canMakeMove(c)) {
                moves.add(c);
            }
        }
        return moves;
    }

    public void minimaxDecision(int depth, GameStateModule game) {
        HashMap<Integer, Integer> vals = new HashMap<Integer, Integer>();
        int max = -Integer.MAX_VALUE;
        int move = 0;
        if (useStatic) {
            for (int c : availableMoves(this.staticGame)) {
                vals.put(maxValue(depth, this.staticGame), c);
            }
        } else {
            for (int c : availableMoves(game)) {
                vals.put(maxValue(depth, game), c);
            }
        }
        for (Map.Entry<Integer, Integer> m : vals.entrySet()) {
            if (m.getKey() > max) {
                move = m.getValue();
            }
        }
        this.chosenMove = move;
    }

    public int maxValue(int depth, GameStateModule game) {
        if (depth == 0 || this.terminate || game.isGameOver()) {
            if (depth == 0) {
                this.staticGame = game.copy();
                this.useStatic = true;
            } else {
                this.useStatic = false;
            }
            return utility(game);
        }

        int v = -Integer.MAX_VALUE;
        int temp;
        for (int c : availableMoves(game)) {
            game.makeMove(c);
            temp = minValue(depth, game);
            v = Math.max(temp, v);
            game.unMakeMove();
        }

        return v;
    }

    public int minValue(int depth, GameStateModule game) {
        if (depth == 0 || this.terminate || game.isGameOver()) {
            if (depth == 0) {
                this.staticGame = game.copy();
                this.useStatic = true;
            } else {
                this.useStatic = false;
            }
            return utility(game);
        }

        int v = Integer.MAX_VALUE;
        int temp;
        for (int c : availableMoves(game)) {
            game.makeMove(c);
            // Only update the depth here.
            // Then we'll always have a max static board.
            temp = maxValue(depth - 1, game);
            v = Math.min(temp, v);
            game.unMakeMove();
        }

        return v;
    }

    public int utility(GameStateModule game) {
        if (game.isGameOver()) {
            if (game.getWinner() == this.player) {
                return 1;
            } else {
                return -1;
            }
        } else {
            return 0;
        }
    }
}
