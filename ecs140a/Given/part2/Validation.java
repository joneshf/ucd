/**
 * Provides a disjoint union of two types.
 * A Validation is either <code>Invalid</code> or it is <code>Valid</code>.
 */
public abstract class Validation<I, V> {

    public static <I, V> Validation<I, V> valid(final V valid) {
        return new Valid<I, V>(valid);
    }

    public static <I, V> Validation<I, V> invalid(final I invalid) {
        return new Invalid<I, V>(invalid);
    }

    /**
     * Tests whether a <code>Validation</code> is invalid.
     *
     * @return <code>true</code> if this is invalid and
     *         <code>false</code> otherwise.
     */
    abstract public boolean isInvalid();

    /**
     * Tests whether a <code>Validation</code> is valid.
     *
     * @return <code>true</code> if this is valid and
     *         <code>false</code> otherwise.
     */
    abstract public boolean isValid();

    abstract public Object value();

    private static final class Invalid<I, V> extends Validation<I, V> {

        private final I invalid;

        Invalid (final I invalid) {
            this.invalid = invalid;
        }

        public I value() {
            return this.invalid;
        }

        public boolean isInvalid() {
            return true;
        }

        public boolean isValid() {
            return false;
        }
    }

    private static final class Valid<I, V> extends Validation<I, V> {

        private final V valid;

        Valid (V valid) {
            this.valid = valid;
        }

        public V value() {
            return this.valid;
        }

        public boolean isInvalid() {
            return false;
        }

        public boolean isValid() {
            return true;
        }
    }
}
