package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.UnsupportedEncodingException;
import java.util.zip.ZipException;
import org.apache.tools.zip.ExtraFieldUtils.UnparseableExtraField;
import org.junit.Test;

public class ExtraFieldUtilsDiffblueTest {
  /**
   * Test {@link ExtraFieldUtils#register(Class)}.
   * <ul>
   *   <li>When {@code Object}.</li>
   *   <li>Then throw {@link RuntimeException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#register(Class)}
   */
  @Test
  public void testRegister_whenJavaLangObject_thenThrowRuntimeException() {
    // Arrange
    Class<Object> c = Object.class;

    // Act and Assert
    assertThrows(RuntimeException.class, () -> ExtraFieldUtils.register(c));
  }

  /**
   * Test {@link ExtraFieldUtils#register(Class)}.
   * <ul>
   *   <li>When {@code ZipExtraField}.</li>
   *   <li>Then throw {@link RuntimeException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#register(Class)}
   */
  @Test
  public void testRegister_whenOrgApacheToolsZipZipExtraField_thenThrowRuntimeException() {
    // Arrange
    Class<ZipExtraField> c = ZipExtraField.class;

    // Act and Assert
    assertThrows(RuntimeException.class, () -> ExtraFieldUtils.register(c));
  }

  /**
   * Test {@link ExtraFieldUtils#createExtraField(ZipShort)}.
   * <ul>
   *   <li>Then return {@link UnrecognizedExtraField}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#createExtraField(ZipShort)}
   */
  @Test
  public void testCreateExtraField_thenReturnUnrecognizedExtraField()
      throws IllegalAccessException, InstantiationException {
    // Arrange
    ZipShort headerId = new ZipShort(42);

    // Act
    ZipExtraField actualCreateExtraFieldResult = ExtraFieldUtils.createExtraField(headerId);

    // Assert
    assertTrue(actualCreateExtraFieldResult instanceof UnrecognizedExtraField);
    assertNull(actualCreateExtraFieldResult.getCentralDirectoryData());
    assertNull(actualCreateExtraFieldResult.getLocalFileDataData());
    assertSame(headerId, actualCreateExtraFieldResult.getHeaderId());
  }

  /**
   * Test {@link ExtraFieldUtils#createExtraField(ZipShort)}.
   * <ul>
   *   <li>Then return {@link Zip64ExtendedInformationExtraField}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#createExtraField(ZipShort)}
   */
  @Test
  public void testCreateExtraField_thenReturnZip64ExtendedInformationExtraField()
      throws IllegalAccessException, InstantiationException {
    // Arrange and Act
    ZipExtraField actualCreateExtraFieldResult = ExtraFieldUtils.createExtraField(new ZipShort(1));

    // Assert
    assertTrue(actualCreateExtraFieldResult instanceof Zip64ExtendedInformationExtraField);
    assertNull(((Zip64ExtendedInformationExtraField) actualCreateExtraFieldResult).getCompressedSize());
    assertNull(((Zip64ExtendedInformationExtraField) actualCreateExtraFieldResult).getRelativeHeaderOffset());
    assertNull(((Zip64ExtendedInformationExtraField) actualCreateExtraFieldResult).getSize());
    assertNull(((Zip64ExtendedInformationExtraField) actualCreateExtraFieldResult).getDiskStartNumber());
    ZipShort centralDirectoryLength = actualCreateExtraFieldResult.getCentralDirectoryLength();
    assertEquals(0, centralDirectoryLength.getValue());
    assertEquals(centralDirectoryLength, actualCreateExtraFieldResult.getLocalFileDataLength());
    ZipShort expectedHeaderId = ((Zip64ExtendedInformationExtraField) actualCreateExtraFieldResult).HEADER_ID;
    assertSame(expectedHeaderId, actualCreateExtraFieldResult.getHeaderId());
    assertArrayEquals(new byte[]{}, actualCreateExtraFieldResult.getCentralDirectoryData());
    assertArrayEquals(new byte[]{}, actualCreateExtraFieldResult.getLocalFileDataData());
    assertArrayEquals(new byte[]{0, 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test {@link ExtraFieldUtils#createExtraField(ZipShort)}.
   * <ul>
   *   <li>When {@link UnicodeCommentExtraField#UCOM_ID}.</li>
   *   <li>Then return {@link UnicodeCommentExtraField}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#createExtraField(ZipShort)}
   */
  @Test
  public void testCreateExtraField_whenUcom_id_thenReturnUnicodeCommentExtraField()
      throws IllegalAccessException, InstantiationException {
    // Arrange and Act
    ZipExtraField actualCreateExtraFieldResult = ExtraFieldUtils.createExtraField(UnicodeCommentExtraField.UCOM_ID);

    // Assert
    assertTrue(actualCreateExtraFieldResult instanceof UnicodeCommentExtraField);
    assertNull(((UnicodeCommentExtraField) actualCreateExtraFieldResult).getUnicodeName());
    assertNull(actualCreateExtraFieldResult.getCentralDirectoryData());
    assertNull(actualCreateExtraFieldResult.getLocalFileDataData());
    assertEquals(0L, ((UnicodeCommentExtraField) actualCreateExtraFieldResult).getNameCRC32());
    ZipShort expectedHeaderId = ((UnicodeCommentExtraField) actualCreateExtraFieldResult).UCOM_ID;
    assertSame(expectedHeaderId, actualCreateExtraFieldResult.getHeaderId());
  }

  /**
   * Test {@link ExtraFieldUtils#parse(byte[], boolean, UnparseableExtraField)} with {@code data}, {@code local}, {@code onUnparseableData}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#parse(byte[], boolean, UnparseableExtraField)}
   */
  @Test
  public void testParseWithDataLocalOnUnparseableData_thenReturnArrayLengthIsZero() throws ZipException {
    // Arrange, Act and Assert
    assertEquals(0, ExtraFieldUtils.parse(new byte[]{}, true, null).length);
  }

  /**
   * Test {@link ExtraFieldUtils#parse(byte[], boolean)} with {@code data}, {@code local}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link ZipException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#parse(byte[], boolean)}
   */
  @Test
  public void testParseWithDataLocal_whenAxaxaxaxBytesIsUtf8_thenThrowZipException()
      throws UnsupportedEncodingException, ZipException {
    // Arrange, Act and Assert
    assertThrows(ZipException.class, () -> ExtraFieldUtils.parse("AXAXAXAX".getBytes("UTF-8"), true));
  }

  /**
   * Test {@link ExtraFieldUtils#parse(byte[], boolean)} with {@code data}, {@code local}.
   * <ul>
   *   <li>When empty array of {@code byte}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#parse(byte[], boolean)}
   */
  @Test
  public void testParseWithDataLocal_whenEmptyArrayOfByte_thenReturnArrayLengthIsZero() throws ZipException {
    // Arrange, Act and Assert
    assertEquals(0, ExtraFieldUtils.parse(new byte[]{}, true).length);
  }

  /**
   * Test {@link ExtraFieldUtils#parse(byte[])} with {@code data}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link ZipException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#parse(byte[])}
   */
  @Test
  public void testParseWithData_whenAxaxaxaxBytesIsUtf8_thenThrowZipException()
      throws UnsupportedEncodingException, ZipException {
    // Arrange, Act and Assert
    assertThrows(ZipException.class, () -> ExtraFieldUtils.parse("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link ExtraFieldUtils#parse(byte[])} with {@code data}.
   * <ul>
   *   <li>When empty array of {@code byte}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#parse(byte[])}
   */
  @Test
  public void testParseWithData_whenEmptyArrayOfByte_thenReturnArrayLengthIsZero() throws ZipException {
    // Arrange, Act and Assert
    assertEquals(0, ExtraFieldUtils.parse(new byte[]{}).length);
  }

  /**
   * Test {@link ExtraFieldUtils#mergeLocalFileDataData(ZipExtraField[])}.
   * <ul>
   *   <li>Given {@code A}.</li>
   *   <li>Then return array of {@code byte} with {@code A} and one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#mergeLocalFileDataData(ZipExtraField[])}
   */
  @Test
  public void testMergeLocalFileDataData_givenA_thenReturnArrayOfByteWithAAndOne() {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromLocalFileData(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}, 2, 3);

    // Act and Assert
    assertArrayEquals(new byte[]{'A', 1, 'A'},
        ExtraFieldUtils.mergeLocalFileDataData(new ZipExtraField[]{unparseableExtraFieldData}));
  }

  /**
   * Test {@link ExtraFieldUtils#mergeLocalFileDataData(ZipExtraField[])}.
   * <ul>
   *   <li>Then return array of {@code byte} with minus two and minus fifty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#mergeLocalFileDataData(ZipExtraField[])}
   */
  @Test
  public void testMergeLocalFileDataData_thenReturnArrayOfByteWithMinusTwoAndMinusFiftyFour() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{-2, -54, 0, 0},
        ExtraFieldUtils.mergeLocalFileDataData(new ZipExtraField[]{JarMarker.getInstance()}));
  }

  /**
   * Test {@link ExtraFieldUtils#mergeLocalFileDataData(ZipExtraField[])}.
   * <ul>
   *   <li>Then return array of {@code byte} with {@code n} and {@code u}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#mergeLocalFileDataData(ZipExtraField[])}
   */
  @Test
  public void testMergeLocalFileDataData_thenReturnArrayOfByteWithNAndU() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        ExtraFieldUtils.mergeLocalFileDataData(new ZipExtraField[]{new AsiExtraField()}));
  }

  /**
   * Test {@link ExtraFieldUtils#mergeLocalFileDataData(ZipExtraField[])}.
   * <ul>
   *   <li>Then return empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#mergeLocalFileDataData(ZipExtraField[])}
   */
  @Test
  public void testMergeLocalFileDataData_thenReturnEmptyArrayOfByte() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{}, ExtraFieldUtils.mergeLocalFileDataData(new ZipExtraField[]{}));
  }

  /**
   * Test {@link ExtraFieldUtils#mergeCentralDirectoryData(ZipExtraField[])}.
   * <ul>
   *   <li>Given {@code A}.</li>
   *   <li>Then return array of {@code byte} with {@code A} and one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#mergeCentralDirectoryData(ZipExtraField[])}
   */
  @Test
  public void testMergeCentralDirectoryData_givenA_thenReturnArrayOfByteWithAAndOne() {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromLocalFileData(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}, 2, 3);

    // Act and Assert
    assertArrayEquals(new byte[]{'A', 1, 'A'},
        ExtraFieldUtils.mergeCentralDirectoryData(new ZipExtraField[]{unparseableExtraFieldData}));
  }

  /**
   * Test {@link ExtraFieldUtils#mergeCentralDirectoryData(ZipExtraField[])}.
   * <ul>
   *   <li>Then return array of {@code byte} with minus two and minus fifty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#mergeCentralDirectoryData(ZipExtraField[])}
   */
  @Test
  public void testMergeCentralDirectoryData_thenReturnArrayOfByteWithMinusTwoAndMinusFiftyFour() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{-2, -54, 0, 0},
        ExtraFieldUtils.mergeCentralDirectoryData(new ZipExtraField[]{JarMarker.getInstance()}));
  }

  /**
   * Test {@link ExtraFieldUtils#mergeCentralDirectoryData(ZipExtraField[])}.
   * <ul>
   *   <li>Then return array of {@code byte} with {@code n} and {@code u}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#mergeCentralDirectoryData(ZipExtraField[])}
   */
  @Test
  public void testMergeCentralDirectoryData_thenReturnArrayOfByteWithNAndU() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        ExtraFieldUtils.mergeCentralDirectoryData(new ZipExtraField[]{new AsiExtraField()}));
  }

  /**
   * Test {@link ExtraFieldUtils#mergeCentralDirectoryData(ZipExtraField[])}.
   * <ul>
   *   <li>Then return empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtraFieldUtils#mergeCentralDirectoryData(ZipExtraField[])}
   */
  @Test
  public void testMergeCentralDirectoryData_thenReturnEmptyArrayOfByte() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{}, ExtraFieldUtils.mergeCentralDirectoryData(new ZipExtraField[]{}));
  }
}
