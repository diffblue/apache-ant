package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.UnsupportedEncodingException;
import java.util.zip.ZipException;
import org.junit.Test;

public class AbstractUnicodeExtraFieldDiffblueTest {
  /**
   * Test {@link AbstractUnicodeExtraField#getNameCRC32()}.
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getNameCRC32()}
   */
  @Test
  public void testGetNameCRC32() {
    // Arrange, Act and Assert
    assertEquals(0L, (new UnicodeCommentExtraField()).getNameCRC32());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#setNameCRC32(long)}.
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#setNameCRC32(long)}
   */
  @Test
  public void testSetNameCRC32() {
    // Arrange
    UnicodeCommentExtraField unicodeCommentExtraField = new UnicodeCommentExtraField();

    // Act
    unicodeCommentExtraField.setNameCRC32(1L);

    // Assert
    assertEquals(1L, unicodeCommentExtraField.getNameCRC32());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getUnicodeName()}.
   * <ul>
   *   <li>Given {@link UnicodeCommentExtraField#UnicodeCommentExtraField()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getUnicodeName()}
   */
  @Test
  public void testGetUnicodeName_givenUnicodeCommentExtraField_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new UnicodeCommentExtraField()).getUnicodeName());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getUnicodeName()}.
   * <ul>
   *   <li>Then return {@code Comment} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getUnicodeName()}
   */
  @Test
  public void testGetUnicodeName_thenReturnCommentBytesIsUtf8() throws UnsupportedEncodingException {
    // Arrange and Act
    byte[] actualUnicodeName = (new UnicodeCommentExtraField("Comment", "AXAXAXAX".getBytes("UTF-8"))).getUnicodeName();

    // Assert
    assertArrayEquals("Comment".getBytes("UTF-8"), actualUnicodeName);
  }

  /**
   * Test {@link AbstractUnicodeExtraField#setUnicodeName(byte[])}.
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#setUnicodeName(byte[])}
   */
  @Test
  public void testSetUnicodeName() throws UnsupportedEncodingException {
    // Arrange
    UnicodeCommentExtraField unicodeCommentExtraField = new UnicodeCommentExtraField();

    // Act
    unicodeCommentExtraField.setUnicodeName("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    ZipShort centralDirectoryLength = unicodeCommentExtraField.getCentralDirectoryLength();
    assertEquals(13, centralDirectoryLength.getValue());
    assertEquals(centralDirectoryLength, unicodeCommentExtraField.getLocalFileDataLength());
    byte[] expectedUnicodeName = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedUnicodeName, unicodeCommentExtraField.getUnicodeName());
    assertArrayEquals(new byte[]{'\r', 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{1, 0, 0, 0, 0, 'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'},
        unicodeCommentExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{1, 0, 0, 0, 0, 'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'},
        unicodeCommentExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#setUnicodeName(byte[])}.
   * <ul>
   *   <li>Then {@link UnicodeCommentExtraField#UnicodeCommentExtraField()} CentralDirectoryData is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#setUnicodeName(byte[])}
   */
  @Test
  public void testSetUnicodeName_thenUnicodeCommentExtraFieldCentralDirectoryDataIsNull() {
    // Arrange
    UnicodeCommentExtraField unicodeCommentExtraField = new UnicodeCommentExtraField();

    // Act
    unicodeCommentExtraField.setUnicodeName(null);

    // Assert that nothing has changed
    assertNull(unicodeCommentExtraField.getCentralDirectoryData());
    assertNull(unicodeCommentExtraField.getLocalFileDataData());
    assertNull(unicodeCommentExtraField.getUnicodeName());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getCentralDirectoryData()}.
   * <ul>
   *   <li>Given {@link UnicodeCommentExtraField#UnicodeCommentExtraField()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData_givenUnicodeCommentExtraField_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new UnicodeCommentExtraField()).getCentralDirectoryData());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getCentralDirectoryData()}.
   * <ul>
   *   <li>Then return array of {@code byte} with one and minus one hundred four.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData_thenReturnArrayOfByteWithOneAndMinusOneHundredFour()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{1, -104, 1, '<', 17, 'T', 'e', 'x', 't'},
        (new UnicodeCommentExtraField("Text", "AXAXAXAX".getBytes("UTF-8"), 1, 3)).getCentralDirectoryData());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getCentralDirectoryData()}.
   * <ul>
   *   <li>Then return array of {@code byte} with one and minus thirty-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData_thenReturnArrayOfByteWithOneAndMinusThirtySeven()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{1, -37, 0, -99, -111, 'C', 'o', 'm', 'm', 'e', 'n', 't'},
        (new UnicodeCommentExtraField("Comment", "AXAXAXAX".getBytes("UTF-8"))).getCentralDirectoryData());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getCentralDirectoryLength()}.
   * <ul>
   *   <li>Then return Value is twelve.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength_thenReturnValueIsTwelve() throws UnsupportedEncodingException {
    // Arrange and Act
    ZipShort actualCentralDirectoryLength = (new UnicodeCommentExtraField("Comment", "AXAXAXAX".getBytes("UTF-8")))
        .getCentralDirectoryLength();

    // Assert
    assertEquals(12, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{'\f', 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getLocalFileDataData()}.
   * <ul>
   *   <li>Given {@link UnicodeCommentExtraField#UnicodeCommentExtraField()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_givenUnicodeCommentExtraField_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new UnicodeCommentExtraField()).getLocalFileDataData());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getLocalFileDataData()}.
   * <ul>
   *   <li>Then return array of {@code byte} with one and minus thirty-seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_thenReturnArrayOfByteWithOneAndMinusThirtySeven()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{1, -37, 0, -99, -111, 'C', 'o', 'm', 'm', 'e', 'n', 't'},
        (new UnicodeCommentExtraField("Comment", "AXAXAXAX".getBytes("UTF-8"))).getLocalFileDataData());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#getLocalFileDataLength()}.
   * <ul>
   *   <li>Then return Value is twelve.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#getLocalFileDataLength()}
   */
  @Test
  public void testGetLocalFileDataLength_thenReturnValueIsTwelve() throws UnsupportedEncodingException {
    // Arrange and Act
    ZipShort actualLocalFileDataLength = (new UnicodeCommentExtraField("Comment", "AXAXAXAX".getBytes("UTF-8")))
        .getLocalFileDataLength();

    // Assert
    assertEquals(12, actualLocalFileDataLength.getValue());
    assertArrayEquals(new byte[]{'\f', 0}, actualLocalFileDataLength.getBytes());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#parseFromLocalFileData(byte[], int, int)}.
   * <ul>
   *   <li>Then {@link UnicodeCommentExtraField#UnicodeCommentExtraField()} NameCRC32 is {@code 1096302936}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#parseFromLocalFileData(byte[], int, int)}
   */
  @Test
  public void testParseFromLocalFileData_thenUnicodeCommentExtraFieldNameCRC32Is1096302936() throws ZipException {
    // Arrange
    UnicodeCommentExtraField unicodeCommentExtraField = new UnicodeCommentExtraField();

    // Act
    unicodeCommentExtraField.parseFromLocalFileData(new byte[]{'A', 'X', 1, 'X', 'A', 'X', 'A', 'X'}, 2, 5);

    // Assert
    assertEquals(1096302936L, unicodeCommentExtraField.getNameCRC32());
    ZipShort centralDirectoryLength = unicodeCommentExtraField.getCentralDirectoryLength();
    assertEquals(5, centralDirectoryLength.getValue());
    assertEquals(centralDirectoryLength, unicodeCommentExtraField.getLocalFileDataLength());
    assertArrayEquals(new byte[]{}, unicodeCommentExtraField.getUnicodeName());
    assertArrayEquals(new byte[]{5, 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{1, 'X', 'A', 'X', 'A'}, unicodeCommentExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{1, 'X', 'A', 'X', 'A'}, unicodeCommentExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link AbstractUnicodeExtraField#parseFromLocalFileData(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link ZipException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#parseFromLocalFileData(byte[], int, int)}
   */
  @Test
  public void testParseFromLocalFileData_whenAxaxaxaxBytesIsUtf8_thenThrowZipException()
      throws UnsupportedEncodingException, ZipException {
    // Arrange
    UnicodeCommentExtraField unicodeCommentExtraField = new UnicodeCommentExtraField();

    // Act and Assert
    assertThrows(ZipException.class,
        () -> unicodeCommentExtraField.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link AbstractUnicodeExtraField#parseFromLocalFileData(byte[], int, int)}.
   * <ul>
   *   <li>When five.</li>
   *   <li>Then throw {@link ZipException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractUnicodeExtraField#parseFromLocalFileData(byte[], int, int)}
   */
  @Test
  public void testParseFromLocalFileData_whenFive_thenThrowZipException() throws ZipException {
    // Arrange, Act and Assert
    assertThrows(ZipException.class, () -> (new UnicodeCommentExtraField())
        .parseFromLocalFileData(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, 2, 5));
  }
}
