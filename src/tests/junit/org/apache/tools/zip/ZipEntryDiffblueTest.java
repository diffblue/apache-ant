package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.nio.file.Paths;
import java.util.NoSuchElementException;
import java.util.zip.ZipException;
import org.junit.Test;

public class ZipEntryDiffblueTest {
  /**
   * Test {@link ZipEntry#ZipEntry()}.
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry()}
   */
  @Test
  public void testNewZipEntry() {
    // Arrange and Act
    ZipEntry actualZipEntry = new ZipEntry();

    // Assert
    assertEquals("", actualZipEntry.getName());
    assertNull(actualZipEntry.getExtra());
    assertNull(actualZipEntry.getRawName());
    assertNull(actualZipEntry.getComment());
    assertNull(actualZipEntry.getCreationTime());
    assertNull(actualZipEntry.getLastAccessTime());
    assertNull(actualZipEntry.getLastModifiedTime());
    assertNull(actualZipEntry.getUnparseableExtraFieldData());
    assertEquals(-1L, actualZipEntry.getCompressedSize());
    assertEquals(-1L, actualZipEntry.getCrc());
    assertEquals(-1L, actualZipEntry.getTime());
    assertEquals(-1L, actualZipEntry.getSize());
    assertEquals(0, actualZipEntry.getInternalAttributes());
    assertEquals(0, actualZipEntry.getPlatform());
    assertEquals(0, actualZipEntry.getUnixMode());
    assertEquals(0, actualZipEntry.getExtraFields().length);
    assertEquals(0L, actualZipEntry.getExternalAttributes());
    assertEquals(ZipEntry.CRC_UNKNOWN, actualZipEntry.getMethod());
    assertArrayEquals(new byte[]{}, actualZipEntry.getCentralDirectoryExtra());
    assertArrayEquals(new byte[]{}, actualZipEntry.getLocalFileDataExtra());
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>Given {@link AsiExtraField} (default constructor).</li>
   *   <li>When {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(java.util.zip.ZipEntry)}
   */
  @Test
  public void testNewZipEntry_givenAsiExtraField_whenZipEntryAddExtraFieldAsiExtraField() throws ZipException {
    // Arrange
    ZipEntry entry = new ZipEntry();
    entry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new ZipEntry((java.util.zip.ZipEntry) entry));
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>Given {@link AsiExtraField} (default constructor).</li>
   *   <li>When {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(ZipEntry)}
   */
  @Test
  public void testNewZipEntry_givenAsiExtraField_whenZipEntryAddExtraFieldAsiExtraField2() throws ZipException {
    // Arrange
    ZipEntry entry = new ZipEntry();
    entry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new ZipEntry(entry));
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>Given one.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Method is one.</li>
   *   <li>Then return Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(java.util.zip.ZipEntry)}
   */
  @Test
  public void testNewZipEntry_givenOne_whenZipEntryMethodIsOne_thenReturnNameIsEmptyString() throws ZipException {
    // Arrange
    ZipEntry entry = new ZipEntry();
    entry.setMethod(1);
    entry.addExtraField(new AsiExtraField());

    // Act
    ZipEntry actualZipEntry = new ZipEntry((java.util.zip.ZipEntry) entry);

    // Assert
    assertEquals("", actualZipEntry.getName());
    assertNull(actualZipEntry.getRawName());
    assertNull(actualZipEntry.getComment());
    assertNull(actualZipEntry.getCreationTime());
    assertNull(actualZipEntry.getLastAccessTime());
    assertNull(actualZipEntry.getLastModifiedTime());
    assertNull(actualZipEntry.getUnparseableExtraFieldData());
    assertEquals(-1L, actualZipEntry.getCompressedSize());
    assertEquals(-1L, actualZipEntry.getCrc());
    assertEquals(-1L, actualZipEntry.getTime());
    assertEquals(-1L, actualZipEntry.getSize());
    assertEquals(0, actualZipEntry.getInternalAttributes());
    assertEquals(0, actualZipEntry.getPlatform());
    assertEquals(0, actualZipEntry.getUnixMode());
    assertEquals(0L, actualZipEntry.getExternalAttributes());
    assertEquals(1, actualZipEntry.getMethod());
    assertEquals(1, actualZipEntry.getExtraFields().length);
    byte[] extra = actualZipEntry.getExtra();
    assertSame(extra, actualZipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, '9', 'h', -33, -123, 0, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        extra);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, '9', 'h', -33, -123, 0, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        actualZipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>Given one.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Method is one.</li>
   *   <li>Then return Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(ZipEntry)}
   */
  @Test
  public void testNewZipEntry_givenOne_whenZipEntryMethodIsOne_thenReturnNameIsEmptyString2() throws ZipException {
    // Arrange
    ZipEntry entry = new ZipEntry();
    entry.setMethod(1);
    entry.addExtraField(new AsiExtraField());

    // Act
    ZipEntry actualZipEntry = new ZipEntry(entry);

    // Assert
    assertEquals("", actualZipEntry.getName());
    assertNull(actualZipEntry.getRawName());
    assertNull(actualZipEntry.getComment());
    assertNull(actualZipEntry.getCreationTime());
    assertNull(actualZipEntry.getLastAccessTime());
    assertNull(actualZipEntry.getLastModifiedTime());
    assertNull(actualZipEntry.getUnparseableExtraFieldData());
    assertEquals(-1L, actualZipEntry.getCompressedSize());
    assertEquals(-1L, actualZipEntry.getCrc());
    assertEquals(-1L, actualZipEntry.getTime());
    assertEquals(-1L, actualZipEntry.getSize());
    assertEquals(0, actualZipEntry.getInternalAttributes());
    assertEquals(0, actualZipEntry.getPlatform());
    assertEquals(0, actualZipEntry.getUnixMode());
    assertEquals(0L, actualZipEntry.getExternalAttributes());
    assertEquals(1, actualZipEntry.getMethod());
    assertEquals(1, actualZipEntry.getExtraFields().length);
    byte[] extra = actualZipEntry.getExtra();
    assertSame(extra, actualZipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, '9', 'h', -33, -123, 0, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        extra);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, '9', 'h', -33, -123, 0, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        actualZipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#ZipEntry(File, String)}.
   * <ul>
   *   <li>Then return Name is {@code Entry Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(File, String)}
   */
  @Test
  public void testNewZipEntry_thenReturnNameIsEntryName() {
    // Arrange and Act
    ZipEntry actualZipEntry = new ZipEntry(Paths.get(System.getProperty("java.io.tmpdir"), "42", "foo").toFile(),
        "Entry Name");

    // Assert
    assertEquals("Entry Name", actualZipEntry.getName());
    assertEquals(0L, actualZipEntry.getLastModifiedTime().toMillis());
    assertEquals(0L, actualZipEntry.getTime());
    assertArrayEquals(new byte[]{}, actualZipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#ZipEntry(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(String)}
   */
  @Test
  public void testNewZipEntry_whenName_thenReturnName() {
    // Arrange and Act
    ZipEntry actualZipEntry = new ZipEntry("Name");

    // Assert
    assertEquals("Name", actualZipEntry.getName());
    assertNull(actualZipEntry.getExtra());
    assertNull(actualZipEntry.getRawName());
    assertNull(actualZipEntry.getComment());
    assertNull(actualZipEntry.getCreationTime());
    assertNull(actualZipEntry.getLastAccessTime());
    assertNull(actualZipEntry.getLastModifiedTime());
    assertNull(actualZipEntry.getUnparseableExtraFieldData());
    assertEquals(-1L, actualZipEntry.getCompressedSize());
    assertEquals(-1L, actualZipEntry.getCrc());
    assertEquals(-1L, actualZipEntry.getTime());
    assertEquals(-1L, actualZipEntry.getSize());
    assertEquals(0, actualZipEntry.getInternalAttributes());
    assertEquals(0, actualZipEntry.getPlatform());
    assertEquals(0, actualZipEntry.getUnixMode());
    assertEquals(0, actualZipEntry.getExtraFields().length);
    assertEquals(0L, actualZipEntry.getExternalAttributes());
    assertEquals(ZipEntry.CRC_UNKNOWN, actualZipEntry.getMethod());
    assertArrayEquals(new byte[]{}, actualZipEntry.getCentralDirectoryExtra());
    assertArrayEquals(new byte[]{}, actualZipEntry.getLocalFileDataExtra());
  }

  /**
   * Test {@link ZipEntry#ZipEntry(String)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then return Name is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(String)}
   */
  @Test
  public void testNewZipEntry_whenSlash_thenReturnNameIsSlash() {
    // Arrange and Act
    ZipEntry actualZipEntry = new ZipEntry("/");

    // Assert
    assertEquals("/", actualZipEntry.getName());
    assertNull(actualZipEntry.getExtra());
    assertNull(actualZipEntry.getRawName());
    assertNull(actualZipEntry.getComment());
    assertNull(actualZipEntry.getCreationTime());
    assertNull(actualZipEntry.getLastAccessTime());
    assertNull(actualZipEntry.getLastModifiedTime());
    assertNull(actualZipEntry.getUnparseableExtraFieldData());
    assertEquals(-1L, actualZipEntry.getCompressedSize());
    assertEquals(-1L, actualZipEntry.getCrc());
    assertEquals(-1L, actualZipEntry.getTime());
    assertEquals(-1L, actualZipEntry.getSize());
    assertEquals(0, actualZipEntry.getInternalAttributes());
    assertEquals(0, actualZipEntry.getPlatform());
    assertEquals(0, actualZipEntry.getUnixMode());
    assertEquals(0, actualZipEntry.getExtraFields().length);
    assertEquals(0L, actualZipEntry.getExternalAttributes());
    assertEquals(ZipEntry.CRC_UNKNOWN, actualZipEntry.getMethod());
    assertArrayEquals(new byte[]{}, actualZipEntry.getCentralDirectoryExtra());
    assertArrayEquals(new byte[]{}, actualZipEntry.getLocalFileDataExtra());
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>When {@link java.util.zip.ZipEntry#ZipEntry(String)} with {@code foo}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(java.util.zip.ZipEntry)}
   */
  @Test
  public void testNewZipEntry_whenZipEntryWithFoo_thenThrowIllegalArgumentException() throws ZipException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new ZipEntry(new java.util.zip.ZipEntry("foo")));
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry(String)} with name is {@code /}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(ZipEntry)}
   */
  @Test
  public void testNewZipEntry_whenZipEntryWithNameIsSlash_thenThrowIllegalArgumentException() throws ZipException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new ZipEntry(new ZipEntry("/")));
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>When {@link java.util.zip.ZipEntry#ZipEntry(String)} with {@code /}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(java.util.zip.ZipEntry)}
   */
  @Test
  public void testNewZipEntry_whenZipEntryWithSlash_thenThrowIllegalArgumentException() throws ZipException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new ZipEntry(new java.util.zip.ZipEntry("/")));
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(java.util.zip.ZipEntry)}
   */
  @Test
  public void testNewZipEntry_whenZipEntry_thenThrowIllegalArgumentException() throws ZipException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new ZipEntry((java.util.zip.ZipEntry) new ZipEntry()));
  }

  /**
   * Test {@link ZipEntry#ZipEntry(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#ZipEntry(ZipEntry)}
   */
  @Test
  public void testNewZipEntry_whenZipEntry_thenThrowIllegalArgumentException2() throws ZipException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new ZipEntry(new ZipEntry()));
  }

  /**
   * Test {@link ZipEntry#clone()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField Instance.</li>
   *   <li>Then return first element is Instance.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#clone()}
   */
  @Test
  public void testClone_givenZipEntryAddExtraFieldInstance_thenReturnFirstElementIsInstance() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    JarMarker ze = JarMarker.getInstance();
    zipEntry.addExtraField(ze);

    // Act
    Object actualCloneResult = zipEntry.clone();

    // Assert
    assertTrue(actualCloneResult instanceof ZipEntry);
    ZipExtraField[] extraFields = ((ZipEntry) actualCloneResult).getExtraFields();
    assertEquals(1, extraFields.length);
    assertSame(ze, extraFields[0]);
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, ((ZipEntry) actualCloneResult).getExtra());
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, ((ZipEntry) actualCloneResult).getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#clone()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return {@link ZipEntry#ZipEntry()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#clone()}
   */
  @Test
  public void testClone_givenZipEntry_thenReturnZipEntry() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    Object actualCloneResult = zipEntry.clone();

    // Assert
    assertTrue(actualCloneResult instanceof ZipEntry);
    assertEquals(zipEntry, actualCloneResult);
  }

  /**
   * Test {@link ZipEntry#clone()}.
   * <ul>
   *   <li>Then return first element is {@link AsiExtraField} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#clone()}
   */
  @Test
  public void testClone_thenReturnFirstElementIsAsiExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    AsiExtraField ze = new AsiExtraField();
    zipEntry.addExtraField(ze);

    // Act
    Object actualCloneResult = zipEntry.clone();

    // Assert
    assertTrue(actualCloneResult instanceof ZipEntry);
    ZipExtraField[] extraFields = ((ZipEntry) actualCloneResult).getExtraFields();
    assertEquals(1, extraFields.length);
    assertSame(ze, extraFields[0]);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        ((ZipEntry) actualCloneResult).getExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        ((ZipEntry) actualCloneResult).getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setMethod(int)}.
   * <ul>
   *   <li>When {@link ZipEntry#CRC_UNKNOWN}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setMethod(int)}
   */
  @Test
  public void testSetMethod_whenCrc_unknown_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new ZipEntry()).setMethod(ZipEntry.CRC_UNKNOWN));
  }

  /**
   * Test {@link ZipEntry#setMethod(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Method is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setMethod(int)}
   */
  @Test
  public void testSetMethod_whenOne_thenZipEntryMethodIsOne() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setMethod(1);

    // Assert
    assertEquals(1, zipEntry.getMethod());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipEntry#setExternalAttributes(long)}
   *   <li>{@link ZipEntry#setGeneralPurposeBit(GeneralPurposeBit)}
   *   <li>{@link ZipEntry#setInternalAttributes(int)}
   *   <li>{@link ZipEntry#setPlatform(int)}
   *   <li>{@link ZipEntry#getExternalAttributes()}
   *   <li>{@link ZipEntry#getGeneralPurposeBit()}
   *   <li>{@link ZipEntry#getInternalAttributes()}
   *   <li>{@link ZipEntry#getMethod()}
   *   <li>{@link ZipEntry#getPlatform()}
   *   <li>{@link ZipEntry#getSize()}
   *   <li>{@link ZipEntry#getUnparseableExtraFieldData()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setExternalAttributes(42L);
    GeneralPurposeBit b = new GeneralPurposeBit();
    zipEntry.setGeneralPurposeBit(b);
    zipEntry.setInternalAttributes(42);
    zipEntry.setPlatform(1);
    long actualExternalAttributes = zipEntry.getExternalAttributes();
    GeneralPurposeBit actualGeneralPurposeBit = zipEntry.getGeneralPurposeBit();
    int actualInternalAttributes = zipEntry.getInternalAttributes();
    int actualMethod = zipEntry.getMethod();
    int actualPlatform = zipEntry.getPlatform();
    long actualSize = zipEntry.getSize();

    // Assert
    assertNull(zipEntry.getUnparseableExtraFieldData());
    assertEquals(-1L, actualSize);
    assertEquals(1, actualPlatform);
    assertEquals(42, actualInternalAttributes);
    assertEquals(42L, actualExternalAttributes);
    assertEquals(ZipEntry.CRC_UNKNOWN, actualMethod);
    assertSame(b, actualGeneralPurposeBit);
  }

  /**
   * Test {@link ZipEntry#setUnixMode(int)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Name is {@code null}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} ExternalAttributes is {@code 65537}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setUnixMode(int)}
   */
  @Test
  public void testSetUnixMode_givenZipEntryNameIsNull_thenZipEntryExternalAttributesIs65537() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setName(null);

    // Act
    zipEntry.setUnixMode(1);

    // Assert
    assertEquals(1, zipEntry.getUnixMode());
    assertEquals(3, zipEntry.getPlatform());
    assertEquals(65537L, zipEntry.getExternalAttributes());
  }

  /**
   * Test {@link ZipEntry#setUnixMode(int)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Name is {@code /}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} ExternalAttributes is {@code 65553}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setUnixMode(int)}
   */
  @Test
  public void testSetUnixMode_givenZipEntryNameIsSlash_thenZipEntryExternalAttributesIs65553() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setName("/");

    // Act
    zipEntry.setUnixMode(1);

    // Assert
    assertEquals(1, zipEntry.getUnixMode());
    assertEquals(3, zipEntry.getPlatform());
    assertEquals(65553L, zipEntry.getExternalAttributes());
  }

  /**
   * Test {@link ZipEntry#setUnixMode(int)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When one.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} ExternalAttributes is {@code 65537}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setUnixMode(int)}
   */
  @Test
  public void testSetUnixMode_givenZipEntry_whenOne_thenZipEntryExternalAttributesIs65537() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setUnixMode(1);

    // Assert
    assertEquals(1, zipEntry.getUnixMode());
    assertEquals(3, zipEntry.getPlatform());
    assertEquals(65537L, zipEntry.getExternalAttributes());
  }

  /**
   * Test {@link ZipEntry#setUnixMode(int)}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} UnixMode is one hundred twenty-eight.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setUnixMode(int)}
   */
  @Test
  public void testSetUnixMode_thenZipEntryUnixModeIsOneHundredTwentyEight() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setUnixMode(128);

    // Assert
    assertEquals(128, zipEntry.getUnixMode());
    assertEquals(3, zipEntry.getPlatform());
    assertEquals(8388608L, zipEntry.getExternalAttributes());
  }

  /**
   * Test {@link ZipEntry#getUnixMode()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} UnixMode is three.</li>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getUnixMode()}
   */
  @Test
  public void testGetUnixMode_givenZipEntryUnixModeIsThree_thenReturnThree() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setUnixMode(3);

    // Act and Assert
    assertEquals(3, zipEntry.getUnixMode());
  }

  /**
   * Test {@link ZipEntry#getUnixMode()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getUnixMode()}
   */
  @Test
  public void testGetUnixMode_givenZipEntry_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new ZipEntry()).getUnixMode());
  }

  /**
   * Test {@link ZipEntry#setExtraFields(ZipExtraField[])}.
   * <p>
   * Method under test: {@link ZipEntry#setExtraFields(ZipExtraField[])}
   */
  @Test
  public void testSetExtraFields() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromLocalFileData(new byte[]{'A', 18, 'A', 18, 'A', 18, 'A', 18}, 2, 3);
    UnparseableExtraFieldData unparseableExtraFieldData2 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData3 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData4 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData5 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData6 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData7 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData8 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData9 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData10 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData11 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData12 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData13 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData14 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData15 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData16 = new UnparseableExtraFieldData();
    UnparseableExtraFieldData unparseableExtraFieldData17 = new UnparseableExtraFieldData();

    // Act
    zipEntry.setExtraFields(new ZipExtraField[]{unparseableExtraFieldData2, unparseableExtraFieldData3,
        unparseableExtraFieldData4, unparseableExtraFieldData5, unparseableExtraFieldData6, unparseableExtraFieldData7,
        unparseableExtraFieldData8, unparseableExtraFieldData9, unparseableExtraFieldData10,
        unparseableExtraFieldData11, unparseableExtraFieldData12, unparseableExtraFieldData13,
        unparseableExtraFieldData14, unparseableExtraFieldData15, unparseableExtraFieldData16,
        unparseableExtraFieldData17, new UnparseableExtraFieldData(), unparseableExtraFieldData});

    // Assert
    assertSame(unparseableExtraFieldData, zipEntry.getUnparseableExtraFieldData());
    assertArrayEquals(new byte[]{'A', 18, 'A'}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{'A', 18, 'A'}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setExtraFields(ZipExtraField[])}.
   * <ul>
   *   <li>Then first element is {@link AsiExtraField} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtraFields(ZipExtraField[])}
   */
  @Test
  public void testSetExtraFields_thenFirstElementIsAsiExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    AsiExtraField asiExtraField = new AsiExtraField();

    // Act
    zipEntry.setExtraFields(new ZipExtraField[]{asiExtraField});

    // Assert
    ZipExtraField[] extraFields = zipEntry.getExtraFields();
    assertEquals(1, extraFields.length);
    assertSame(asiExtraField, extraFields[0]);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setExtraFields(ZipExtraField[])}.
   * <ul>
   *   <li>Then first element is Instance.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtraFields(ZipExtraField[])}
   */
  @Test
  public void testSetExtraFields_thenFirstElementIsInstance() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    JarMarker instance = JarMarker.getInstance();

    // Act
    zipEntry.setExtraFields(new ZipExtraField[]{instance});

    // Assert
    ZipExtraField[] extraFields = zipEntry.getExtraFields();
    assertEquals(1, extraFields.length);
    assertSame(instance, extraFields[0]);
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setExtraFields(ZipExtraField[])}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} UnparseableExtraFieldData is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtraFields(ZipExtraField[])}
   */
  @Test
  public void testSetExtraFields_thenZipEntryUnparseableExtraFieldDataIsNull() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setExtraFields(new ZipExtraField[]{});

    // Assert
    assertNull(zipEntry.getUnparseableExtraFieldData());
    assertEquals(0, zipEntry.getExtraFields().length);
    assertArrayEquals(new byte[]{}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#getExtraFields(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getExtraFields(boolean)}
   */
  @Test
  public void testGetExtraFieldsWithBoolean_givenZipEntry_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new ZipEntry()).getExtraFields(false).length);
  }

  /**
   * Test {@link ZipEntry#getExtraFields(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getExtraFields(boolean)}
   */
  @Test
  public void testGetExtraFieldsWithBoolean_givenZipEntry_whenTrue_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new ZipEntry()).getExtraFields(true).length);
  }

  /**
   * Test {@link ZipEntry#getExtraFields(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Then first element return {@link AsiExtraField}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getExtraFields(boolean)}
   */
  @Test
  public void testGetExtraFieldsWithBoolean_thenFirstElementReturnAsiExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act
    ZipExtraField[] actualExtraFields = zipEntry.getExtraFields(true);

    // Assert
    ZipExtraField zipExtraField = actualExtraFields[0];
    assertTrue(zipExtraField instanceof AsiExtraField);
    assertEquals(1, actualExtraFields.length);
    ZipShort centralDirectoryLength = zipExtraField.getCentralDirectoryLength();
    assertEquals(centralDirectoryLength, zipExtraField.getLocalFileDataLength());
    assertArrayEquals(new byte[]{14, 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link ZipEntry#getExtraFields(boolean)} with {@code boolean}.
   * <ul>
   *   <li>Then first element return {@link AsiExtraField}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getExtraFields(boolean)}
   */
  @Test
  public void testGetExtraFieldsWithBoolean_thenFirstElementReturnAsiExtraField2() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act
    ZipExtraField[] actualExtraFields = zipEntry.getExtraFields(false);

    // Assert
    ZipExtraField zipExtraField = actualExtraFields[0];
    assertTrue(zipExtraField instanceof AsiExtraField);
    assertEquals(1, actualExtraFields.length);
    ZipShort centralDirectoryLength = zipExtraField.getCentralDirectoryLength();
    assertEquals(centralDirectoryLength, zipExtraField.getLocalFileDataLength());
    assertArrayEquals(new byte[]{14, 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link ZipEntry#getExtraFields()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getExtraFields()}
   */
  @Test
  public void testGetExtraFields_givenZipEntry_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new ZipEntry()).getExtraFields().length);
  }

  /**
   * Test {@link ZipEntry#getExtraFields()}.
   * <ul>
   *   <li>Then return array length is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getExtraFields()}
   */
  @Test
  public void testGetExtraFields_thenReturnArrayLengthIsOne() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    AsiExtraField ze = new AsiExtraField();
    zipEntry.addExtraField(ze);

    // Act
    ZipExtraField[] actualExtraFields = zipEntry.getExtraFields();

    // Assert
    assertEquals(1, actualExtraFields.length);
    assertSame(ze, actualExtraFields[0]);
  }

  /**
   * Test {@link ZipEntry#addExtraField(ZipExtraField)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#addExtraField(ZipExtraField)}
   */
  @Test
  public void testAddExtraField_givenZipEntryAddExtraFieldAsiExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());
    AsiExtraField ze = new AsiExtraField();

    // Act
    zipEntry.addExtraField(ze);

    // Assert that nothing has changed
    ZipExtraField[] extraFields = zipEntry.getExtraFields();
    assertEquals(1, extraFields.length);
    assertSame(ze, extraFields[0]);
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, extra);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#addExtraField(ZipExtraField)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When Instance.</li>
   *   <li>Then first element is Instance.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#addExtraField(ZipExtraField)}
   */
  @Test
  public void testAddExtraField_givenZipEntry_whenInstance_thenFirstElementIsInstance() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    JarMarker ze = JarMarker.getInstance();

    // Act
    zipEntry.addExtraField(ze);

    // Assert
    ZipExtraField[] extraFields = zipEntry.getExtraFields();
    assertEquals(1, extraFields.length);
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertSame(ze, extraFields[0]);
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, extra);
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#addExtraField(ZipExtraField)}.
   * <ul>
   *   <li>When {@link AsiExtraField} (default constructor).</li>
   *   <li>Then first element is {@link AsiExtraField} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#addExtraField(ZipExtraField)}
   */
  @Test
  public void testAddExtraField_whenAsiExtraField_thenFirstElementIsAsiExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    AsiExtraField ze = new AsiExtraField();

    // Act
    zipEntry.addExtraField(ze);

    // Assert
    ZipExtraField[] extraFields = zipEntry.getExtraFields();
    assertEquals(1, extraFields.length);
    assertSame(ze, extraFields[0]);
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, extra);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#addAsFirstExtraField(ZipExtraField)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#addAsFirstExtraField(ZipExtraField)}
   */
  @Test
  public void testAddAsFirstExtraField_givenZipEntryAddExtraFieldAsiExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());
    AsiExtraField ze = new AsiExtraField();

    // Act
    zipEntry.addAsFirstExtraField(ze);

    // Assert that nothing has changed
    ZipExtraField[] extraFields = zipEntry.getExtraFields();
    assertEquals(1, extraFields.length);
    assertSame(ze, extraFields[0]);
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, extra);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#addAsFirstExtraField(ZipExtraField)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When Instance.</li>
   *   <li>Then first element is Instance.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#addAsFirstExtraField(ZipExtraField)}
   */
  @Test
  public void testAddAsFirstExtraField_givenZipEntry_whenInstance_thenFirstElementIsInstance() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    JarMarker ze = JarMarker.getInstance();

    // Act
    zipEntry.addAsFirstExtraField(ze);

    // Assert
    ZipExtraField[] extraFields = zipEntry.getExtraFields();
    assertEquals(1, extraFields.length);
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertSame(ze, extraFields[0]);
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, extra);
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#addAsFirstExtraField(ZipExtraField)}.
   * <ul>
   *   <li>When {@link AsiExtraField} (default constructor).</li>
   *   <li>Then first element is {@link AsiExtraField} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#addAsFirstExtraField(ZipExtraField)}
   */
  @Test
  public void testAddAsFirstExtraField_whenAsiExtraField_thenFirstElementIsAsiExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    AsiExtraField ze = new AsiExtraField();

    // Act
    zipEntry.addAsFirstExtraField(ze);

    // Assert
    ZipExtraField[] extraFields = zipEntry.getExtraFields();
    assertEquals(1, extraFields.length);
    assertSame(ze, extraFields[0]);
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, extra);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#removeExtraField(ZipShort)}.
   * <p>
   * Method under test: {@link ZipEntry#removeExtraField(ZipShort)}
   */
  @Test
  public void testRemoveExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry("Name");
    zipEntry.addExtraField(JarMarker.getInstance());
    zipEntry.addExtraField(new UnicodeCommentExtraField("Comment", new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act
    zipEntry.removeExtraField(UnicodeCommentExtraField.UCOM_ID);

    // Assert
    assertEquals(1, zipEntry.getExtraFields().length);
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#removeExtraField(ZipShort)}.
   * <ul>
   *   <li>Given {@code A}.</li>
   *   <li>When {@link UnicodeCommentExtraField#UCOM_ID}.</li>
   *   <li>Then array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#removeExtraField(ZipShort)}
   */
  @Test
  public void testRemoveExtraField_givenA_whenUcom_id_thenArrayLengthIsZero() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry("Name");
    zipEntry.addExtraField(new UnicodeCommentExtraField("Comment", new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act
    zipEntry.removeExtraField(UnicodeCommentExtraField.UCOM_ID);

    // Assert
    assertEquals(0, zipEntry.getExtraFields().length);
    assertArrayEquals(new byte[]{}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#removeExtraField(ZipShort)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When {@link UnicodeCommentExtraField#UCOM_ID}.</li>
   *   <li>Then throw {@link NoSuchElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#removeExtraField(ZipShort)}
   */
  @Test
  public void testRemoveExtraField_givenZipEntry_whenUcom_id_thenThrowNoSuchElementException() {
    // Arrange, Act and Assert
    assertThrows(NoSuchElementException.class,
        () -> (new ZipEntry()).removeExtraField(UnicodeCommentExtraField.UCOM_ID));
  }

  /**
   * Test {@link ZipEntry#removeExtraField(ZipShort)}.
   * <ul>
   *   <li>Then throw {@link NoSuchElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#removeExtraField(ZipShort)}
   */
  @Test
  public void testRemoveExtraField_thenThrowNoSuchElementException() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertThrows(NoSuchElementException.class, () -> zipEntry.removeExtraField(UnicodeCommentExtraField.UCOM_ID));
  }

  /**
   * Test {@link ZipEntry#removeExtraField(ZipShort)}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry(String)} with {@code Name} Extra is array of {@code byte} with {@code n} and {@code u}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#removeExtraField(ZipShort)}
   */
  @Test
  public void testRemoveExtraField_thenZipEntryWithNameExtraIsArrayOfByteWithNAndU() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry("Name");
    zipEntry.addExtraField(new AsiExtraField());
    zipEntry.addExtraField(new UnicodeCommentExtraField("Comment", new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1}));

    // Act
    zipEntry.removeExtraField(UnicodeCommentExtraField.UCOM_ID);

    // Assert
    assertEquals(1, zipEntry.getExtraFields().length);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#removeUnparseableExtraFieldData()}.
   * <p>
   * Method under test: {@link ZipEntry#removeUnparseableExtraFieldData()}
   */
  @Test
  public void testRemoveUnparseableExtraFieldData() {
    // Arrange, Act and Assert
    assertThrows(NoSuchElementException.class, () -> (new ZipEntry()).removeUnparseableExtraFieldData());
  }

  /**
   * Test {@link ZipEntry#getExtraField(ZipShort)}.
   * <p>
   * Method under test: {@link ZipEntry#getExtraField(ZipShort)}
   */
  @Test
  public void testGetExtraField() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry("Name");
    UnicodeCommentExtraField ze = new UnicodeCommentExtraField("Comment", new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1});

    zipEntry.addExtraField(ze);

    // Act and Assert
    assertSame(ze, zipEntry.getExtraField(UnicodeCommentExtraField.UCOM_ID));
  }

  /**
   * Test {@link ZipEntry#getExtraField(ZipShort)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getExtraField(ZipShort)}
   */
  @Test
  public void testGetExtraField_givenZipEntryAddExtraFieldAsiExtraField_thenReturnNull() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNull(zipEntry.getExtraField(UnicodeCommentExtraField.UCOM_ID));
  }

  /**
   * Test {@link ZipEntry#getExtraField(ZipShort)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When {@link UnicodeCommentExtraField#UCOM_ID}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getExtraField(ZipShort)}
   */
  @Test
  public void testGetExtraField_givenZipEntry_whenUcom_id_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new ZipEntry()).getExtraField(UnicodeCommentExtraField.UCOM_ID));
  }

  /**
   * Test {@link ZipEntry#setExtra(byte[])} with {@code byte[]}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtra(byte[])}
   */
  @Test
  public void testSetExtraWithByte_givenZipEntry_thenZipEntryExtraIsAxaxaxaxBytesIsUtf8()
      throws UnsupportedEncodingException, RuntimeException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setExtra("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    byte[] expectedExtra = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedExtra, zipEntry.getExtra());
    byte[] expectedCentralDirectoryData = "AXAXAXAX".getBytes("UTF-8");
    UnparseableExtraFieldData unparseableExtraFieldData = zipEntry.getUnparseableExtraFieldData();
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    byte[] expectedCentralDirectoryExtra = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryExtra, zipEntry.getCentralDirectoryExtra());
    assertArrayEquals(new byte[]{-63, -84}, unparseableExtraFieldData.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'\b', 0}, unparseableExtraFieldData.getCentralDirectoryLength().getBytes());
  }

  /**
   * Test {@link ZipEntry#setExtra(byte[])} with {@code byte[]}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is array of {@code byte} with minus two and minus fifty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtra(byte[])}
   */
  @Test
  public void testSetExtraWithByte_thenZipEntryExtraIsArrayOfByteWithMinusTwoAndMinusFiftyFour()
      throws UnsupportedEncodingException, RuntimeException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(JarMarker.getInstance());

    // Act
    zipEntry.setExtra("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    byte[] expectedCentralDirectoryData = "AXAXAXAX".getBytes("UTF-8");
    UnparseableExtraFieldData unparseableExtraFieldData = zipEntry.getUnparseableExtraFieldData();
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    assertArrayEquals(new byte[]{-63, -84}, unparseableExtraFieldData.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'\b', 0}, unparseableExtraFieldData.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{-2, -54, 0, 0, 'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{-2, -54, 0, 0, 'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setExtra(byte[])} with {@code byte[]}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is array of {@code byte} with {@code n} and {@code u}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtra(byte[])}
   */
  @Test
  public void testSetExtraWithByte_thenZipEntryExtraIsArrayOfByteWithNAndU()
      throws UnsupportedEncodingException, RuntimeException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act
    zipEntry.setExtra("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    byte[] expectedCentralDirectoryData = "AXAXAXAX".getBytes("UTF-8");
    UnparseableExtraFieldData unparseableExtraFieldData = zipEntry.getUnparseableExtraFieldData();
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    assertArrayEquals(new byte[]{-63, -84}, unparseableExtraFieldData.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'\b', 0}, unparseableExtraFieldData.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'A', 'X', 'A', 'X',
        'A', 'X', 'A', 'X'}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'A', 'X', 'A', 'X',
        'A', 'X', 'A', 'X'}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setExtra(byte[])} with {@code byte[]}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} LocalFileDataExtra is {@link ZipEntry#ZipEntry()} Extra.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtra(byte[])}
   */
  @Test
  public void testSetExtraWithByte_thenZipEntryLocalFileDataExtraIsZipEntryExtra() throws RuntimeException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act
    zipEntry.setExtra(new byte[]{});

    // Assert that nothing has changed
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, extra);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setExtra(byte[])} with {@code byte[]}.
   * <ul>
   *   <li>When empty array of {@code byte}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtra(byte[])}
   */
  @Test
  public void testSetExtraWithByte_whenEmptyArrayOfByte_thenZipEntryExtraIsEmptyArrayOfByte() throws RuntimeException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setExtra(new byte[]{});

    // Assert
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{}, extra);
    assertArrayEquals(new byte[]{}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setExtra()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtra()}
   */
  @Test
  public void testSetExtra_givenZipEntry_thenZipEntryExtraIsEmptyArrayOfByte() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setExtra();

    // Assert
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{}, extra);
  }

  /**
   * Test {@link ZipEntry#setExtra()}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is array of {@code byte} with minus two and minus fifty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtra()}
   */
  @Test
  public void testSetExtra_thenZipEntryExtraIsArrayOfByteWithMinusTwoAndMinusFiftyFour() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(JarMarker.getInstance());

    // Act
    zipEntry.setExtra();

    // Assert that nothing has changed
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, extra);
  }

  /**
   * Test {@link ZipEntry#setExtra()}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is array of {@code byte} with {@code n} and {@code u}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setExtra()}
   */
  @Test
  public void testSetExtra_thenZipEntryExtraIsArrayOfByteWithNAndU() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act
    zipEntry.setExtra();

    // Assert that nothing has changed
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, extra);
  }

  /**
   * Test {@link ZipEntry#setCentralDirectoryExtra(byte[])}.
   * <p>
   * Method under test: {@link ZipEntry#setCentralDirectoryExtra(byte[])}
   */
  @Test
  public void testSetCentralDirectoryExtra() throws UnsupportedEncodingException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(JarMarker.getInstance());

    // Act
    zipEntry.setCentralDirectoryExtra("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    byte[] expectedCentralDirectoryData = "AXAXAXAX".getBytes("UTF-8");
    UnparseableExtraFieldData unparseableExtraFieldData = zipEntry.getUnparseableExtraFieldData();
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    assertArrayEquals(new byte[]{-63, -84}, unparseableExtraFieldData.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'\b', 0}, unparseableExtraFieldData.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{-2, -54, 0, 0, 'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{-2, -54, 0, 0, 'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setCentralDirectoryExtra(byte[])}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is array of {@code byte} with {@code n} and {@code u}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setCentralDirectoryExtra(byte[])}
   */
  @Test
  public void testSetCentralDirectoryExtra_thenZipEntryExtraIsArrayOfByteWithNAndU()
      throws UnsupportedEncodingException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act
    zipEntry.setCentralDirectoryExtra("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    byte[] expectedCentralDirectoryData = "AXAXAXAX".getBytes("UTF-8");
    UnparseableExtraFieldData unparseableExtraFieldData = zipEntry.getUnparseableExtraFieldData();
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    assertArrayEquals(new byte[]{-63, -84}, unparseableExtraFieldData.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'\b', 0}, unparseableExtraFieldData.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'A', 'X', 'A', 'X',
        'A', 'X', 'A', 'X'}, zipEntry.getExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'A', 'X', 'A', 'X',
        'A', 'X', 'A', 'X'}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setCentralDirectoryExtra(byte[])}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setCentralDirectoryExtra(byte[])}
   */
  @Test
  public void testSetCentralDirectoryExtra_thenZipEntryExtraIsAxaxaxaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setCentralDirectoryExtra("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    byte[] expectedExtra = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedExtra, zipEntry.getExtra());
    byte[] expectedCentralDirectoryData = "AXAXAXAX".getBytes("UTF-8");
    UnparseableExtraFieldData unparseableExtraFieldData = zipEntry.getUnparseableExtraFieldData();
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    byte[] expectedCentralDirectoryExtra = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryExtra, zipEntry.getCentralDirectoryExtra());
    assertArrayEquals(new byte[]{-63, -84}, unparseableExtraFieldData.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'\b', 0}, unparseableExtraFieldData.getCentralDirectoryLength().getBytes());
  }

  /**
   * Test {@link ZipEntry#setCentralDirectoryExtra(byte[])}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setCentralDirectoryExtra(byte[])}
   */
  @Test
  public void testSetCentralDirectoryExtra_thenZipEntryExtraIsEmptyArrayOfByte() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setCentralDirectoryExtra(new byte[]{});

    // Assert
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{}, extra);
    assertArrayEquals(new byte[]{}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setCentralDirectoryExtra(byte[])}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} LocalFileDataExtra is {@link ZipEntry#ZipEntry()} Extra.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setCentralDirectoryExtra(byte[])}
   */
  @Test
  public void testSetCentralDirectoryExtra_thenZipEntryLocalFileDataExtraIsZipEntryExtra() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act
    zipEntry.setCentralDirectoryExtra(new byte[]{});

    // Assert that nothing has changed
    byte[] extra = zipEntry.getExtra();
    assertSame(extra, zipEntry.getLocalFileDataExtra());
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, extra);
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#getLocalFileDataExtra()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getLocalFileDataExtra()}
   */
  @Test
  public void testGetLocalFileDataExtra_givenZipEntry_thenReturnEmptyArrayOfByte() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{}, (new ZipEntry()).getLocalFileDataExtra());
  }

  /**
   * Test {@link ZipEntry#getLocalFileDataExtra()}.
   * <ul>
   *   <li>Then return array of {@code byte} with {@code n} and {@code u}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getLocalFileDataExtra()}
   */
  @Test
  public void testGetLocalFileDataExtra_thenReturnArrayOfByteWithNAndU() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getLocalFileDataExtra());
  }

  /**
   * Test {@link ZipEntry#getCentralDirectoryExtra()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getCentralDirectoryExtra()}
   */
  @Test
  public void testGetCentralDirectoryExtra_givenZipEntry_thenReturnEmptyArrayOfByte() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{}, (new ZipEntry()).getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#getCentralDirectoryExtra()}.
   * <ul>
   *   <li>Then return array of {@code byte} with minus two and minus fifty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getCentralDirectoryExtra()}
   */
  @Test
  public void testGetCentralDirectoryExtra_thenReturnArrayOfByteWithMinusTwoAndMinusFiftyFour() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(JarMarker.getInstance());

    // Act and Assert
    assertArrayEquals(new byte[]{-2, -54, 0, 0}, zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#getCentralDirectoryExtra()}.
   * <ul>
   *   <li>Then return array of {@code byte} with {@code n} and {@code u}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getCentralDirectoryExtra()}
   */
  @Test
  public void testGetCentralDirectoryExtra_thenReturnArrayOfByteWithNAndU() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertArrayEquals(new byte[]{'n', 'u', 14, 0, 'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipEntry#setComprSize(long)}.
   * <p>
   * Method under test: {@link ZipEntry#setComprSize(long)}
   */
  @Test
  public void testSetComprSize() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setComprSize(3L);

    // Assert
    assertEquals(3L, zipEntry.getCompressedSize());
  }

  /**
   * Test {@link ZipEntry#getName()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getName()}
   */
  @Test
  public void testGetName_givenZipEntry() {
    // Arrange, Act and Assert
    assertEquals("", (new ZipEntry()).getName());
  }

  /**
   * Test {@link ZipEntry#getName()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Name is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getName()}
   */
  @Test
  public void testGetName_givenZipEntryNameIsNull() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setName(null);

    // Act and Assert
    assertEquals("", zipEntry.getName());
  }

  /**
   * Test {@link ZipEntry#isDirectory()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Name is {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenZipEntryNameIsNull_thenReturnFalse() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setName(null);

    // Act and Assert
    assertFalse(zipEntry.isDirectory());
  }

  /**
   * Test {@link ZipEntry#isDirectory()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Name is {@code /}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenZipEntryNameIsSlash_thenReturnTrue() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setName("/");

    // Act and Assert
    assertTrue(zipEntry.isDirectory());
  }

  /**
   * Test {@link ZipEntry#isDirectory()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenZipEntry_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new ZipEntry()).isDirectory());
  }

  /**
   * Test {@link ZipEntry#setName(String, byte[])} with {@code name}, {@code rawName}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} UnixMode is zero.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Name is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setName(String, byte[])}
   */
  @Test
  public void testSetNameWithNameRawName_givenZipEntryUnixModeIsZero_thenZipEntryNameIsSlash()
      throws UnsupportedEncodingException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setUnixMode(0);

    // Act
    zipEntry.setName("/", new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'});

    // Assert
    assertEquals("/", zipEntry.getName());
    byte[] expectedRawName = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedRawName, zipEntry.getRawName());
  }

  /**
   * Test {@link ZipEntry#setName(String, byte[])} with {@code name}, {@code rawName}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setName(String, byte[])}
   */
  @Test
  public void testSetNameWithNameRawName_givenZipEntry_whenName_thenZipEntryNameIsName()
      throws UnsupportedEncodingException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setName("Name", "AXAXAXAX".getBytes("UTF-8"));

    // Assert
    assertEquals("Name", zipEntry.getName());
    byte[] expectedRawName = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedRawName, zipEntry.getRawName());
  }

  /**
   * Test {@link ZipEntry#setName(String, byte[])} with {@code name}, {@code rawName}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When {@code /}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Name is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setName(String, byte[])}
   */
  @Test
  public void testSetNameWithNameRawName_givenZipEntry_whenSlash_thenZipEntryNameIsSlash()
      throws UnsupportedEncodingException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setName("/", "AXAXAXAX".getBytes("UTF-8"));

    // Assert
    assertEquals("/", zipEntry.getName());
    byte[] expectedRawName = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedRawName, zipEntry.getRawName());
  }

  /**
   * Test {@link ZipEntry#setName(String, byte[])} with {@code name}, {@code rawName}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setName(String, byte[])}
   */
  @Test
  public void testSetNameWithNameRawName_whenNull_thenZipEntryNameIsEmptyString() throws UnsupportedEncodingException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setUnixMode(0);

    // Act
    zipEntry.setName(null, new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'});

    // Assert
    assertEquals("", zipEntry.getName());
    byte[] expectedRawName = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedRawName, zipEntry.getRawName());
  }

  /**
   * Test {@link ZipEntry#setName(String)} with {@code name}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} UnixMode is zero.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Name is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setName(String)}
   */
  @Test
  public void testSetNameWithName_givenZipEntryUnixModeIsZero_thenZipEntryNameIsSlash() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setUnixMode(0);

    // Act
    zipEntry.setName("/");

    // Assert
    assertEquals("/", zipEntry.getName());
  }

  /**
   * Test {@link ZipEntry#setName(String)} with {@code name}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setName(String)}
   */
  @Test
  public void testSetNameWithName_givenZipEntry_whenName_thenZipEntryNameIsName() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setName("Name");

    // Assert
    assertEquals("Name", zipEntry.getName());
  }

  /**
   * Test {@link ZipEntry#setName(String)} with {@code name}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>When {@code /}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Name is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setName(String)}
   */
  @Test
  public void testSetNameWithName_givenZipEntry_whenSlash_thenZipEntryNameIsSlash() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setName("/");

    // Assert
    assertEquals("/", zipEntry.getName());
  }

  /**
   * Test {@link ZipEntry#setName(String)} with {@code name}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setName(String)}
   */
  @Test
  public void testSetNameWithName_whenNull_thenZipEntryNameIsEmptyString() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setUnixMode(0);

    // Act
    zipEntry.setName(null);

    // Assert that nothing has changed
    assertEquals("", zipEntry.getName());
  }

  /**
   * Test {@link ZipEntry#setSize(long)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setSize(long)}
   */
  @Test
  public void testSetSize_whenMinusOne_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> (new ZipEntry()).setSize(-1L));
  }

  /**
   * Test {@link ZipEntry#setSize(long)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#setSize(long)}
   */
  @Test
  public void testSetSize_whenThree_thenZipEntrySizeIsThree() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act
    zipEntry.setSize(3L);

    // Assert
    assertEquals(3L, zipEntry.getSize());
  }

  /**
   * Test {@link ZipEntry#getRawName()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getRawName()}
   */
  @Test
  public void testGetRawName_givenZipEntry_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new ZipEntry()).getRawName());
  }

  /**
   * Test {@link ZipEntry#getRawName()}.
   * <ul>
   *   <li>Then return {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#getRawName()}
   */
  @Test
  public void testGetRawName_thenReturnAxaxaxaxBytesIsUtf8() throws UnsupportedEncodingException {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setName("Name", "AXAXAXAX".getBytes("UTF-8"));

    // Act
    byte[] actualRawName = zipEntry.getRawName();

    // Assert
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), actualRawName);
  }

  /**
   * Test {@link ZipEntry#equals(Object)}, and {@link ZipEntry#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipEntry#equals(Object)}
   *   <li>{@link ZipEntry#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    ZipEntry zipEntry2 = new ZipEntry();

    // Act and Assert
    assertEquals(zipEntry, zipEntry2);
    int expectedHashCodeResult = zipEntry.hashCode();
    assertEquals(expectedHashCodeResult, zipEntry2.hashCode());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}, and {@link ZipEntry#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipEntry#equals(Object)}
   *   <li>{@link ZipEntry#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();

    // Act and Assert
    assertEquals(zipEntry, zipEntry);
    int expectedHashCodeResult = zipEntry.hashCode();
    assertEquals(expectedHashCodeResult, zipEntry.hashCode());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry("Name");

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setMethod(1);
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual4() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setInternalAttributes(42);
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual5() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setExternalAttributes(42L);
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual6() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setUnixMode(1);
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual7() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setSize(3L);
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual8() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setCompressedSize(1L);
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual9() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setCrc(1L);
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual10() {
    // Arrange
    ZipEntry zipEntry = new ZipEntry();
    zipEntry.setComment("foo");
    zipEntry.addExtraField(new AsiExtraField());

    // Act and Assert
    assertNotEquals(zipEntry, new ZipEntry());
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new ZipEntry(), null);
  }

  /**
   * Test {@link ZipEntry#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEntry#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new ZipEntry(), "Different type to ZipEntry");
  }
}
