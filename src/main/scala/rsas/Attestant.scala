package rsas

/**
 * Signs data of type D. Produced signature is of type S.
 * @tparam D Type of signed data.
 * @tparam S Type of signature produced.
 */
trait Attestant[D, S] {

  /**
   * Data signed by an attestant.
   */
  trait Signed {
    /**
     * Data that is signed.
     * @return Data that is signed.
     */
    def data: D

    /**
     * Signature on the data signed.
     * @return Signature on the data signed.
     */
    def signature: S

    /**
     * Attenstant who signed the data.
     * @return Attestant who signed the data.
     */
    def attestant: Attestant[D, S]

    /**
     * Human-readable String representation of the signed data.
     * @return Human-readable String representation.
     */
    override def toString: String = s"$data signed $signature (by $attestant)"
  }

  /**
   * Get the attestant's signature on the data.
   * @param data Data to be signed.
   * @return Attestant's signature on the data.
   */
  def getSignature(data: D): S

  /**
   * Get the Signed data with the attestant's signature.
   * @param dataToSign Data to sign.
   * @return Signed data with the attestant's signature.
   */
  def getSigned(dataToSign: D): Signed =
    new Signed {
      def attestant: Attestant[D, S] = Attestant.this

      def data: D = dataToSign

      def signature: S = getSignature(dataToSign)
    }

  /**
   * Hash function used by this attestant.
   * @param bytes Data to hash.
   * @return Resulting hash.
   */
  def hash(bytes: Seq[Byte]): Seq[Byte]

}
