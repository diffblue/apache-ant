package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.UnsupportedEncodingException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import org.junit.Test;

public class ZipUtilDiffblueTest {
  /**
   * Test {@link ZipUtil#toDosTime(Calendar, long, byte[], int)} with {@code c}, {@code t}, {@code buf}, {@code offset}.
   * <p>
   * Method under test: {@link ZipUtil#toDosTime(Calendar, long, byte[], int)}
   */
  @Test
  public void testToDosTimeWithCTBufOffset() throws UnsupportedEncodingException {
    // Arrange
    GregorianCalendar c = new GregorianCalendar(1, 1, 1);

    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act
    ZipUtil.toDosTime(c, 1L, buf, 2);

    // Assert
    assertEquals(1970, c.getWeekYear());
    assertEquals(1L, c.getTimeInMillis());
    assertEquals(53, c.getWeeksInWeekYear());
    assertArrayEquals(new byte[]{'A', 'X', 0, '!', 0, 0, 'A', 'X'}, buf);
  }

  /**
   * Test {@link ZipUtil#toDosTime(Calendar, long, byte[], int)} with {@code c}, {@code t}, {@code buf}, {@code offset}.
   * <p>
   * Method under test: {@link ZipUtil#toDosTime(Calendar, long, byte[], int)}
   */
  @Test
  public void testToDosTimeWithCTBufOffset2() throws UnsupportedEncodingException {
    // Arrange
    GregorianCalendar c = new GregorianCalendar(1, 1, 1);

    // Act
    ZipUtil.toDosTime(c, Long.MAX_VALUE, "AXAXAXAX".getBytes("UTF-8"), 2);

    // Assert
    assertEquals(292278994, c.getWeekYear());
    assertEquals(52, c.getWeeksInWeekYear());
    assertEquals(Long.MAX_VALUE, c.getTimeInMillis());
  }

  /**
   * Test {@link ZipUtil#toDosTime(long, byte[], int)} with {@code t}, {@code buf}, {@code offset}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#toDosTime(long, byte[], int)}
   */
  @Test
  public void testToDosTimeWithTBufOffset_thenAxaxaxaxBytesIsUtf8IsArrayOfByteWithAAndX()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act
    ZipUtil.toDosTime(1L, buf, 2);

    // Assert
    assertArrayEquals(new byte[]{'A', 'X', 0, '!', 0, 0, 'A', 'X'}, buf);
  }

  /**
   * Test {@link ZipUtil#toDosTime(long)} with {@code t}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return array of {@code byte} with zero and {@code !}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#toDosTime(long)}
   */
  @Test
  public void testToDosTimeWithT_whenOne_thenReturnArrayOfByteWithZeroAndExclamationMark() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{0, '!', 0, 0}, ZipUtil.toDosTime(1L));
  }

  /**
   * Test {@link ZipUtil#toDosTime(Date)} with {@code time}.
   * <ul>
   *   <li>Then return Value is {@code 8448}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#toDosTime(Date)}
   */
  @Test
  public void testToDosTimeWithTime_thenReturnValueIs8448() {
    // Arrange and Act
    ZipLong actualToDosTimeResult = ZipUtil
        .toDosTime(Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant()));

    // Assert
    assertEquals(8448L, actualToDosTimeResult.getValue());
    assertArrayEquals(new byte[]{0, '!', 0, 0}, actualToDosTimeResult.getBytes());
  }

  /**
   * Test {@link ZipUtil#adjustToLong(int)}.
   * <ul>
   *   <li>When {@link ZipEntry#CRC_UNKNOWN}.</li>
   *   <li>Then return {@code 4294967295}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#adjustToLong(int)}
   */
  @Test
  public void testAdjustToLong_whenCrc_unknown_thenReturn4294967295() {
    // Arrange, Act and Assert
    assertEquals(4294967295L, ZipUtil.adjustToLong(ZipEntry.CRC_UNKNOWN));
  }

  /**
   * Test {@link ZipUtil#adjustToLong(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#adjustToLong(int)}
   */
  @Test
  public void testAdjustToLong_whenOne_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1L, ZipUtil.adjustToLong(1));
  }

  /**
   * Test {@link ZipUtil#fromDosTime(ZipLong)}.
   * <ul>
   *   <li>Then return {@link SimpleDateFormat#SimpleDateFormat(String)} with {@code yyyy-MM-dd} format is {@code 1980-12-01}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#fromDosTime(ZipLong)}
   */
  @Test
  public void testFromDosTime_thenReturnSimpleDateFormatWithYyyyMmDdFormatIs19801201() {
    // Arrange and Act
    Date actualFromDosTimeResult = ZipUtil.fromDosTime(ZipLong.CFH_SIG);

    // Assert
    assertEquals("1980-12-01", (new SimpleDateFormat("yyyy-MM-dd")).format(actualFromDosTimeResult));
  }

  /**
   * Test {@link ZipUtil#copy(byte[])}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#copy(byte[])}
   */
  @Test
  public void testCopy_whenAxaxaxaxBytesIsUtf8_thenReturnAxaxaxaxBytesIsUtf8() throws UnsupportedEncodingException {
    // Arrange and Act
    byte[] actualCopyResult = ZipUtil.copy("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), actualCopyResult);
  }

  /**
   * Test {@link ZipUtil#copy(byte[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#copy(byte[])}
   */
  @Test
  public void testCopy_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(ZipUtil.copy(null));
  }

  /**
   * Test {@link ZipUtil#canHandleEntryData(ZipEntry)}.
   * <ul>
   *   <li>Given eight.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Method is eight.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#canHandleEntryData(ZipEntry)}
   */
  @Test
  public void testCanHandleEntryData_givenEight_whenZipEntryMethodIsEight_thenReturnTrue() {
    // Arrange
    GeneralPurposeBit b = new GeneralPurposeBit();
    b.useEncryption(false);

    ZipEntry entry = new ZipEntry();
    entry.setMethod(8);
    entry.setGeneralPurposeBit(b);

    // Act and Assert
    assertTrue(ZipUtil.canHandleEntryData(entry));
  }

  /**
   * Test {@link ZipUtil#canHandleEntryData(ZipEntry)}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor) useEncryption {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#canHandleEntryData(ZipEntry)}
   */
  @Test
  public void testCanHandleEntryData_givenGeneralPurposeBitUseEncryptionFalse_thenReturnTrue() {
    // Arrange
    GeneralPurposeBit b = new GeneralPurposeBit();
    b.useEncryption(false);

    ZipEntry entry = new ZipEntry();
    entry.setMethod(0);
    entry.setGeneralPurposeBit(b);

    // Act and Assert
    assertTrue(ZipUtil.canHandleEntryData(entry));
  }

  /**
   * Test {@link ZipUtil#canHandleEntryData(ZipEntry)}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor) useEncryption {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#canHandleEntryData(ZipEntry)}
   */
  @Test
  public void testCanHandleEntryData_givenGeneralPurposeBitUseEncryptionTrue_thenReturnFalse() {
    // Arrange
    GeneralPurposeBit b = new GeneralPurposeBit();
    b.useEncryption(true);

    ZipEntry entry = new ZipEntry();
    entry.setMethod(0);
    entry.setGeneralPurposeBit(b);

    // Act and Assert
    assertFalse(ZipUtil.canHandleEntryData(entry));
  }

  /**
   * Test {@link ZipUtil#canHandleEntryData(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#canHandleEntryData(ZipEntry)}
   */
  @Test
  public void testCanHandleEntryData_whenZipEntry_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(ZipUtil.canHandleEntryData(new ZipEntry()));
  }

  /**
   * Test {@link ZipUtil#checkRequestedFeatures(ZipEntry)}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor) useEncryption {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#checkRequestedFeatures(ZipEntry)}
   */
  @Test
  public void testCheckRequestedFeatures_givenGeneralPurposeBitUseEncryptionTrue()
      throws UnsupportedZipFeatureException {
    // Arrange
    GeneralPurposeBit b = new GeneralPurposeBit();
    b.useEncryption(true);

    ZipEntry ze = new ZipEntry();
    ze.setMethod(0);
    ze.setGeneralPurposeBit(b);
    ze.setName(null);

    // Act and Assert
    assertThrows(UnsupportedZipFeatureException.class, () -> ZipUtil.checkRequestedFeatures(ze));
  }

  /**
   * Test {@link ZipUtil#checkRequestedFeatures(ZipEntry)}.
   * <ul>
   *   <li>Given three.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Method is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#checkRequestedFeatures(ZipEntry)}
   */
  @Test
  public void testCheckRequestedFeatures_givenThree_whenZipEntryMethodIsThree() throws UnsupportedZipFeatureException {
    // Arrange
    GeneralPurposeBit b = new GeneralPurposeBit();
    b.useEncryption(false);

    ZipEntry ze = new ZipEntry();
    ze.setMethod(3);
    ze.setGeneralPurposeBit(b);
    ze.setName(null);

    // Act and Assert
    assertThrows(UnsupportedZipFeatureException.class, () -> ZipUtil.checkRequestedFeatures(ze));
  }

  /**
   * Test {@link ZipUtil#checkRequestedFeatures(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then throw {@link UnsupportedZipFeatureException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipUtil#checkRequestedFeatures(ZipEntry)}
   */
  @Test
  public void testCheckRequestedFeatures_whenZipEntry_thenThrowUnsupportedZipFeatureException()
      throws UnsupportedZipFeatureException {
    // Arrange, Act and Assert
    assertThrows(UnsupportedZipFeatureException.class, () -> ZipUtil.checkRequestedFeatures(new ZipEntry()));
  }
}
