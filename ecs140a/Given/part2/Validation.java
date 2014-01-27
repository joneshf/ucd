/**
 * Provides a disjoint union of two types.
 * A Validation is either <code>Invalid</code> or it is <code>Valid</code>.
 */
public class Validation<Invalid, Valid> {

    private Invalid invalid = null;
    private Valid valid = null;

    // We use a private constructor to ensure that our invariant
    // of only having one type holds true.
    private Validation(Invalid invalid, Valid valid) {
        this.invalid = invalid;
        this.valid = valid;
    }

    /**
     * Public <code>Invalid</code> constructor.
     *
     * @param i The <code>invalid</code> value.
     */
    public static <Invalid, Valid> Validation<Invalid, Valid> invalid(Invalid i) {
        return new Validation<Invalid, Valid>(i, null);
    }

    /**
     * Public <code>Valid</code> constructor.
     *
     * @param v The <code>valid</code> value.
     */
    public static <Invalid, Valid> Validation<Invalid, Valid> valid(Valid v) {
        return new Validation<Invalid, Valid>(null, v);
    }

    public Invalid invalid() {
        return this.invalid;
    }

    public Valid valid() {
        return this.valid;
    }

    /**
     * Tests whether a <code>Validation</code> is invalid.
     *
     * @return <code>true</code> if this is invalid and
     *         <code>false</code> otherwise.
     */
    public boolean isInvalid() {
        return this.valid == null;
    }

    /**
     * Tests whether a <code>Validation</code> is valid.
     *
     * @return <code>true</code> if this is valid and
     *         <code>false</code> otherwise.
     */
    public boolean isValid() {
        return this.invalid == null;
    }
}
