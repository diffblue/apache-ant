package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class ZipShortDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipShort#ZipShort(int)}
   *   <li>{@link ZipShort#toString()}
   *   <li>{@link ZipShort#getValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ZipShort actualZipShort = new ZipShort(42);
    String actualToStringResult = actualZipShort.toString();

    // Assert
    assertEquals("ZipShort value: 42", actualToStringResult);
    assertEquals(42, actualZipShort.getValue());
  }

  /**
   * Test {@link ZipShort#ZipShort(byte[])}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return Value is {@code 22593}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipShort#ZipShort(byte[])}
   */
  @Test
  public void testNewZipShort_whenAxaxaxaxBytesIsUtf8_thenReturnValueIs22593() throws UnsupportedEncodingException {
    // Arrange and Act
    ZipShort actualZipShort = new ZipShort("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    assertEquals(22593, actualZipShort.getValue());
    assertArrayEquals(new byte[]{'A', 'X'}, actualZipShort.getBytes());
  }

  /**
   * Test {@link ZipShort#ZipShort(byte[], int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return Value is {@code 22593}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipShort#ZipShort(byte[], int)}
   */
  @Test
  public void testNewZipShort_whenAxaxaxaxBytesIsUtf8_thenReturnValueIs225932() throws UnsupportedEncodingException {
    // Arrange and Act
    ZipShort actualZipShort = new ZipShort("AXAXAXAX".getBytes("UTF-8"), 2);

    // Assert
    assertEquals(22593, actualZipShort.getValue());
    assertArrayEquals(new byte[]{'A', 'X'}, actualZipShort.getBytes());
  }

  /**
   * Test {@link ZipShort#getBytes()}.
   * <p>
   * Method under test: {@link ZipShort#getBytes()}
   */
  @Test
  public void testGetBytes() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'u', 'c'}, UnicodeCommentExtraField.UCOM_ID.getBytes());
  }

  /**
   * Test {@link ZipShort#getBytes(int)} with {@code int}.
   * <p>
   * Method under test: {@link ZipShort#getBytes(int)}
   */
  @Test
  public void testGetBytesWithInt() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'*', 0}, ZipShort.getBytes(42));
  }

  /**
   * Test {@link ZipShort#putShort(int, byte[], int)}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipShort#putShort(int, byte[], int)}
   */
  @Test
  public void testPutShort_thenAxaxaxaxBytesIsUtf8IsArrayOfByteWithAAndX() throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act
    ZipShort.putShort(42, buf, 2);

    // Assert
    assertArrayEquals(new byte[]{'A', 'X', '*', 0, 'A', 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link ZipShort#getValue(byte[], int)} with {@code bytes}, {@code offset}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code 22593}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipShort#getValue(byte[], int)}
   */
  @Test
  public void testGetValueWithBytesOffset_whenAxaxaxaxBytesIsUtf8_thenReturn22593()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(22593, ZipShort.getValue("AXAXAXAX".getBytes("UTF-8"), 2));
  }

  /**
   * Test {@link ZipShort#getValue(byte[])} with {@code bytes}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code 22593}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipShort#getValue(byte[])}
   */
  @Test
  public void testGetValueWithBytes_whenAxaxaxaxBytesIsUtf8_thenReturn22593() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(22593, ZipShort.getValue("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link ZipShort#equals(Object)}, and {@link ZipShort#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipShort#equals(Object)}
   *   <li>{@link ZipShort#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    ZipShort zipShort = UnicodeCommentExtraField.UCOM_ID;
    ZipShort zipShort2 = UnicodeCommentExtraField.UCOM_ID;

    // Act and Assert
    assertEquals(zipShort, zipShort2);
    int expectedHashCodeResult = zipShort.hashCode();
    assertEquals(expectedHashCodeResult, zipShort2.hashCode());
  }

  /**
   * Test {@link ZipShort#equals(Object)}, and {@link ZipShort#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipShort#equals(Object)}
   *   <li>{@link ZipShort#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    ZipShort zipShort = UnicodeCommentExtraField.UCOM_ID;

    // Act and Assert
    assertEquals(zipShort, zipShort);
    int expectedHashCodeResult = zipShort.hashCode();
    assertEquals(expectedHashCodeResult, zipShort.hashCode());
  }

  /**
   * Test {@link ZipShort#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipShort#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(UnicodePathExtraField.UPATH_ID, UnicodeCommentExtraField.UCOM_ID);
  }

  /**
   * Test {@link ZipShort#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipShort#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(UnicodeCommentExtraField.UCOM_ID, null);
  }

  /**
   * Test {@link ZipShort#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipShort#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(UnicodeCommentExtraField.UCOM_ID, "Different type to ZipShort");
  }

  /**
   * Test {@link ZipShort#clone()}.
   * <p>
   * Method under test: {@link ZipShort#clone()}
   */
  @Test
  public void testClone() {
    // Arrange
    ZipShort zipShort = UnicodeCommentExtraField.UCOM_ID;

    // Act
    Object actualCloneResult = zipShort.clone();

    // Assert
    assertTrue(actualCloneResult instanceof ZipShort);
    assertEquals(zipShort, actualCloneResult);
  }
}
