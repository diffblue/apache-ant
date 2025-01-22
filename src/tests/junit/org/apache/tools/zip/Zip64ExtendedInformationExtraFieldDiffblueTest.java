package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import java.io.UnsupportedEncodingException;
import java.util.zip.ZipException;
import org.junit.Test;

public class Zip64ExtendedInformationExtraFieldDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Zip64ExtendedInformationExtraField#Zip64ExtendedInformationExtraField()}
   *   <li>{@link Zip64ExtendedInformationExtraField#setCompressedSize(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setDiskStartNumber(ZipLong)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setRelativeHeaderOffset(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setSize(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#getCompressedSize()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getDiskStartNumber()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getHeaderId()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getRelativeHeaderOffset()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getSize()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Zip64ExtendedInformationExtraField actualZip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    actualZip64ExtendedInformationExtraField.setCompressedSize(ZipEightByteInteger.ZERO);
    actualZip64ExtendedInformationExtraField.setDiskStartNumber(ZipLong.CFH_SIG);
    actualZip64ExtendedInformationExtraField.setRelativeHeaderOffset(ZipEightByteInteger.ZERO);
    actualZip64ExtendedInformationExtraField.setSize(ZipEightByteInteger.ZERO);
    ZipEightByteInteger actualCompressedSize = actualZip64ExtendedInformationExtraField.getCompressedSize();
    ZipLong actualDiskStartNumber = actualZip64ExtendedInformationExtraField.getDiskStartNumber();
    ZipShort actualHeaderId = actualZip64ExtendedInformationExtraField.getHeaderId();
    ZipEightByteInteger actualRelativeHeaderOffset = actualZip64ExtendedInformationExtraField.getRelativeHeaderOffset();
    ZipEightByteInteger actualSize = actualZip64ExtendedInformationExtraField.getSize();

    // Assert
    assertSame(actualZip64ExtendedInformationExtraField.HEADER_ID, actualHeaderId);
    ZipEightByteInteger zipEightByteInteger = actualSize.ZERO;
    assertSame(zipEightByteInteger, actualCompressedSize);
    assertSame(zipEightByteInteger, actualRelativeHeaderOffset);
    assertSame(zipEightByteInteger, actualSize);
    assertSame(actualDiskStartNumber.CFH_SIG, actualDiskStartNumber);
  }

  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@link ZipLong#CFH_SIG}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Zip64ExtendedInformationExtraField#Zip64ExtendedInformationExtraField(ZipEightByteInteger, ZipEightByteInteger, ZipEightByteInteger, ZipLong)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setCompressedSize(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setDiskStartNumber(ZipLong)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setRelativeHeaderOffset(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setSize(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#getCompressedSize()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getDiskStartNumber()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getHeaderId()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getRelativeHeaderOffset()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getSize()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenCfh_sig() {
    // Arrange and Act
    Zip64ExtendedInformationExtraField actualZip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField(
        ZipEightByteInteger.ZERO, ZipEightByteInteger.ZERO, ZipEightByteInteger.ZERO, ZipLong.CFH_SIG);
    actualZip64ExtendedInformationExtraField.setCompressedSize(ZipEightByteInteger.ZERO);
    actualZip64ExtendedInformationExtraField.setDiskStartNumber(ZipLong.CFH_SIG);
    actualZip64ExtendedInformationExtraField.setRelativeHeaderOffset(ZipEightByteInteger.ZERO);
    actualZip64ExtendedInformationExtraField.setSize(ZipEightByteInteger.ZERO);
    ZipEightByteInteger actualCompressedSize = actualZip64ExtendedInformationExtraField.getCompressedSize();
    ZipLong actualDiskStartNumber = actualZip64ExtendedInformationExtraField.getDiskStartNumber();
    ZipShort actualHeaderId = actualZip64ExtendedInformationExtraField.getHeaderId();
    ZipEightByteInteger actualRelativeHeaderOffset = actualZip64ExtendedInformationExtraField.getRelativeHeaderOffset();
    ZipEightByteInteger actualSize = actualZip64ExtendedInformationExtraField.getSize();

    // Assert
    assertSame(actualZip64ExtendedInformationExtraField.HEADER_ID, actualHeaderId);
    ZipEightByteInteger zipEightByteInteger = actualSize.ZERO;
    assertSame(zipEightByteInteger, actualCompressedSize);
    assertSame(zipEightByteInteger, actualRelativeHeaderOffset);
    assertSame(zipEightByteInteger, actualSize);
    assertSame(actualDiskStartNumber.CFH_SIG, actualDiskStartNumber);
  }

  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@link ZipEightByteInteger#ZERO}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Zip64ExtendedInformationExtraField#Zip64ExtendedInformationExtraField(ZipEightByteInteger, ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setCompressedSize(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setDiskStartNumber(ZipLong)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setRelativeHeaderOffset(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#setSize(ZipEightByteInteger)}
   *   <li>{@link Zip64ExtendedInformationExtraField#getCompressedSize()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getDiskStartNumber()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getHeaderId()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getRelativeHeaderOffset()}
   *   <li>{@link Zip64ExtendedInformationExtraField#getSize()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenZero() {
    // Arrange and Act
    Zip64ExtendedInformationExtraField actualZip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField(
        ZipEightByteInteger.ZERO, ZipEightByteInteger.ZERO);
    actualZip64ExtendedInformationExtraField.setCompressedSize(ZipEightByteInteger.ZERO);
    actualZip64ExtendedInformationExtraField.setDiskStartNumber(ZipLong.CFH_SIG);
    actualZip64ExtendedInformationExtraField.setRelativeHeaderOffset(ZipEightByteInteger.ZERO);
    actualZip64ExtendedInformationExtraField.setSize(ZipEightByteInteger.ZERO);
    ZipEightByteInteger actualCompressedSize = actualZip64ExtendedInformationExtraField.getCompressedSize();
    ZipLong actualDiskStartNumber = actualZip64ExtendedInformationExtraField.getDiskStartNumber();
    ZipShort actualHeaderId = actualZip64ExtendedInformationExtraField.getHeaderId();
    ZipEightByteInteger actualRelativeHeaderOffset = actualZip64ExtendedInformationExtraField.getRelativeHeaderOffset();
    ZipEightByteInteger actualSize = actualZip64ExtendedInformationExtraField.getSize();

    // Assert
    assertSame(actualZip64ExtendedInformationExtraField.HEADER_ID, actualHeaderId);
    ZipEightByteInteger zipEightByteInteger = actualSize.ZERO;
    assertSame(zipEightByteInteger, actualCompressedSize);
    assertSame(zipEightByteInteger, actualRelativeHeaderOffset);
    assertSame(zipEightByteInteger, actualSize);
    assertSame(actualDiskStartNumber.CFH_SIG, actualDiskStartNumber);
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getLocalFileDataLength()}.
   * <ul>
   *   <li>Then return Value is {@link Short#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getLocalFileDataLength()}
   */
  @Test
  public void testGetLocalFileDataLength_thenReturnValueIsSize() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(ZipEightByteInteger.ZERO);

    // Act
    ZipShort actualLocalFileDataLength = zip64ExtendedInformationExtraField.getLocalFileDataLength();

    // Assert
    assertEquals(Short.SIZE, actualLocalFileDataLength.getValue());
    assertArrayEquals(new byte[]{16, 0}, actualLocalFileDataLength.getBytes());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getLocalFileDataLength()}.
   * <ul>
   *   <li>Then return Value is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getLocalFileDataLength()}
   */
  @Test
  public void testGetLocalFileDataLength_thenReturnValueIsZero() {
    // Arrange and Act
    ZipShort actualLocalFileDataLength = (new Zip64ExtendedInformationExtraField()).getLocalFileDataLength();

    // Assert
    assertEquals(0, actualLocalFileDataLength.getValue());
    assertArrayEquals(new byte[]{0, 0}, actualLocalFileDataLength.getBytes());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}.
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(null);
    zip64ExtendedInformationExtraField.setCompressedSize(null);
    zip64ExtendedInformationExtraField.setRelativeHeaderOffset(ZipEightByteInteger.ZERO);
    zip64ExtendedInformationExtraField.setDiskStartNumber(null);

    // Act
    ZipShort actualCentralDirectoryLength = zip64ExtendedInformationExtraField.getCentralDirectoryLength();

    // Assert
    assertEquals(8, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{'\b', 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}.
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength2() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(null);
    zip64ExtendedInformationExtraField.setCompressedSize(ZipEightByteInteger.ZERO);
    zip64ExtendedInformationExtraField.setRelativeHeaderOffset(null);
    zip64ExtendedInformationExtraField.setDiskStartNumber(null);

    // Act
    ZipShort actualCentralDirectoryLength = zip64ExtendedInformationExtraField.getCentralDirectoryLength();

    // Assert
    assertEquals(8, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{'\b', 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}.
   * <ul>
   *   <li>Given {@link Zip64ExtendedInformationExtraField#Zip64ExtendedInformationExtraField()} Size is {@link ZipEightByteInteger#ZERO}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength_givenZip64ExtendedInformationExtraFieldSizeIsZero() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(ZipEightByteInteger.ZERO);
    zip64ExtendedInformationExtraField.setCompressedSize(null);
    zip64ExtendedInformationExtraField.setRelativeHeaderOffset(null);
    zip64ExtendedInformationExtraField.setDiskStartNumber(null);

    // Act
    ZipShort actualCentralDirectoryLength = zip64ExtendedInformationExtraField.getCentralDirectoryLength();

    // Assert
    assertEquals(8, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{'\b', 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}.
   * <ul>
   *   <li>Then return Value is four.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength_thenReturnValueIsFour() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(null);
    zip64ExtendedInformationExtraField.setCompressedSize(null);
    zip64ExtendedInformationExtraField.setRelativeHeaderOffset(null);
    zip64ExtendedInformationExtraField.setDiskStartNumber(ZipLong.CFH_SIG);

    // Act
    ZipShort actualCentralDirectoryLength = zip64ExtendedInformationExtraField.getCentralDirectoryLength();

    // Assert
    assertEquals(4, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{4, 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}.
   * <ul>
   *   <li>Then return Value is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength_thenReturnValueIsZero() {
    // Arrange and Act
    ZipShort actualCentralDirectoryLength = (new Zip64ExtendedInformationExtraField()).getCentralDirectoryLength();

    // Assert
    assertEquals(0, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{0, 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}.
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(ZipEightByteInteger.ZERO);
    zip64ExtendedInformationExtraField.setCompressedSize(null);

    // Act and Assert
    assertThrows(IllegalArgumentException.class, () -> zip64ExtendedInformationExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}.
   * <ul>
   *   <li>Given {@link Zip64ExtendedInformationExtraField#Zip64ExtendedInformationExtraField()} Size is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_givenZip64ExtendedInformationExtraFieldSizeIsNull() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(null);
    zip64ExtendedInformationExtraField.setCompressedSize(ZipEightByteInteger.ZERO);

    // Act and Assert
    assertThrows(IllegalArgumentException.class, () -> zip64ExtendedInformationExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}.
   * <ul>
   *   <li>Then return array of {@code byte} with minus one and minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_thenReturnArrayOfByteWithMinusOneAndMinusOne() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(new ZipEightByteInteger(-1L));
    zip64ExtendedInformationExtraField.setCompressedSize(ZipEightByteInteger.ZERO);

    // Act and Assert
    assertArrayEquals(new byte[]{-1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0},
        zip64ExtendedInformationExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}.
   * <ul>
   *   <li>Then return array of {@code byte} with zero and zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_thenReturnArrayOfByteWithZeroAndZero() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setSize(ZipEightByteInteger.ZERO);
    zip64ExtendedInformationExtraField.setCompressedSize(ZipEightByteInteger.ZERO);

    // Act and Assert
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zip64ExtendedInformationExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}.
   * <ul>
   *   <li>Then return empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_thenReturnEmptyArrayOfByte() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{}, (new Zip64ExtendedInformationExtraField()).getLocalFileDataData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}.
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setRelativeHeaderOffset(null);
    zip64ExtendedInformationExtraField.setDiskStartNumber(null);
    zip64ExtendedInformationExtraField.setSize(null);
    zip64ExtendedInformationExtraField.setCompressedSize(ZipEightByteInteger.ZERO);

    // Act and Assert
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0}, zip64ExtendedInformationExtraField.getCentralDirectoryData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}.
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData2() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setRelativeHeaderOffset(ZipEightByteInteger.ZERO);
    zip64ExtendedInformationExtraField.setDiskStartNumber(null);
    zip64ExtendedInformationExtraField.setSize(null);
    zip64ExtendedInformationExtraField.setCompressedSize(null);

    // Act and Assert
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0}, zip64ExtendedInformationExtraField.getCentralDirectoryData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}.
   * <ul>
   *   <li>Given {@link Zip64ExtendedInformationExtraField#Zip64ExtendedInformationExtraField()} Size is {@link ZipEightByteInteger#ZERO}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData_givenZip64ExtendedInformationExtraFieldSizeIsZero() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setRelativeHeaderOffset(null);
    zip64ExtendedInformationExtraField.setDiskStartNumber(null);
    zip64ExtendedInformationExtraField.setSize(ZipEightByteInteger.ZERO);
    zip64ExtendedInformationExtraField.setCompressedSize(null);

    // Act and Assert
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0}, zip64ExtendedInformationExtraField.getCentralDirectoryData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}.
   * <ul>
   *   <li>Then return array of {@code byte} with {@code P} and {@code K}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData_thenReturnArrayOfByteWithPAndK() {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();
    zip64ExtendedInformationExtraField.setRelativeHeaderOffset(null);
    zip64ExtendedInformationExtraField.setDiskStartNumber(ZipLong.CFH_SIG);
    zip64ExtendedInformationExtraField.setSize(null);
    zip64ExtendedInformationExtraField.setCompressedSize(null);

    // Act and Assert
    assertArrayEquals(new byte[]{'P', 'K', 1, 2}, zip64ExtendedInformationExtraField.getCentralDirectoryData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}.
   * <ul>
   *   <li>Then return empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData_thenReturnEmptyArrayOfByte() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{}, (new Zip64ExtendedInformationExtraField()).getCentralDirectoryData());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#parseFromLocalFileData(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link ZipException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#parseFromLocalFileData(byte[], int, int)}
   */
  @Test
  public void testParseFromLocalFileData_whenAxaxaxaxBytesIsUtf8_thenThrowZipException()
      throws UnsupportedEncodingException, ZipException {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();

    // Act and Assert
    assertThrows(ZipException.class,
        () -> zip64ExtendedInformationExtraField.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#parseFromCentralDirectoryData(byte[], int, int)}.
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#parseFromCentralDirectoryData(byte[], int, int)}
   */
  @Test
  public void testParseFromCentralDirectoryData() throws UnsupportedEncodingException, ZipException {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();

    // Act
    zip64ExtendedInformationExtraField.parseFromCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Assert that nothing has changed
    ZipShort centralDirectoryLength = zip64ExtendedInformationExtraField.getCentralDirectoryLength();
    assertEquals(0, centralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{}, zip64ExtendedInformationExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test {@link Zip64ExtendedInformationExtraField#parseFromCentralDirectoryData(byte[], int, int)}.
   * <p>
   * Method under test: {@link Zip64ExtendedInformationExtraField#parseFromCentralDirectoryData(byte[], int, int)}
   */
  @Test
  public void testParseFromCentralDirectoryData2() throws UnsupportedEncodingException, ZipException {
    // Arrange
    Zip64ExtendedInformationExtraField zip64ExtendedInformationExtraField = new Zip64ExtendedInformationExtraField();

    // Act
    zip64ExtendedInformationExtraField.parseFromCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"), 2, 4);

    // Assert
    ZipLong diskStartNumber = zip64ExtendedInformationExtraField.getDiskStartNumber();
    assertEquals(1480677441L, diskStartNumber.getValue());
    ZipShort centralDirectoryLength = zip64ExtendedInformationExtraField.getCentralDirectoryLength();
    assertEquals(4, centralDirectoryLength.getValue());
    byte[] expectedCentralDirectoryData = "AXAX".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, zip64ExtendedInformationExtraField.getCentralDirectoryData());
    byte[] expectedBytes = "AXAX".getBytes("UTF-8");
    assertArrayEquals(expectedBytes, diskStartNumber.getBytes());
    assertArrayEquals(new byte[]{4, 0}, centralDirectoryLength.getBytes());
  }
}
