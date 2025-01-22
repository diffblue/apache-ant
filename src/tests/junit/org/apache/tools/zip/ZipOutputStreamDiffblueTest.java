package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.zip.Deflater;
import java.util.zip.ZipException;
import org.junit.Test;

public class ZipOutputStreamDiffblueTest {
  /**
   * Test {@link ZipOutputStream#ZipOutputStream(OutputStream)}.
   * <p>
   * Method under test: {@link ZipOutputStream#ZipOutputStream(OutputStream)}
   */
  @Test
  public void testNewZipOutputStream() {
    // Arrange and Act
    ZipOutputStream actualZipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Assert
    assertNull(actualZipOutputStream.getEncoding());
    assertEquals(512, actualZipOutputStream.buf.length);
    assertFalse(actualZipOutputStream.isSeekable());
  }

  /**
   * Test {@link ZipOutputStream#isSeekable()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#isSeekable()}
   */
  @Test
  public void testIsSeekable_thenReturnFalse() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse((new ZipOutputStream(new ByteArrayOutputStream(1))).isSeekable());
  }

  /**
   * Test {@link ZipOutputStream#setEncoding(String)}.
   * <p>
   * Method under test: {@link ZipOutputStream#setEncoding(String)}
   */
  @Test
  public void testSetEncoding() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zipOutputStream.setEncoding("Encoding");

    // Assert
    assertEquals("Encoding", zipOutputStream.getEncoding());
  }

  /**
   * Test {@link ZipOutputStream#setEncoding(String)}.
   * <ul>
   *   <li>Then {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} Encoding is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#setEncoding(String)}
   */
  @Test
  public void testSetEncoding_thenZipOutputStreamWithOutIsByteArrayOutputStreamEncodingIsNull() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseLanguageEncodingFlag(false);

    // Act
    zipOutputStream.setEncoding(null);

    // Assert that nothing has changed
    assertNull(zipOutputStream.getEncoding());
  }

  /**
   * Test {@link ZipOutputStream#setEncoding(String)}.
   * <ul>
   *   <li>Then {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} Encoding is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#setEncoding(String)}
   */
  @Test
  public void testSetEncoding_thenZipOutputStreamWithOutIsByteArrayOutputStreamEncodingIsUtf8() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zipOutputStream.setEncoding("UTF-8");

    // Assert
    assertEquals("UTF-8", zipOutputStream.getEncoding());
  }

  /**
   * Test {@link ZipOutputStream#setEncoding(String)}.
   * <ul>
   *   <li>Then {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} Encoding is {@code UTF8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#setEncoding(String)}
   */
  @Test
  public void testSetEncoding_thenZipOutputStreamWithOutIsByteArrayOutputStreamEncodingIsUtf82() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zipOutputStream.setEncoding("UTF8");

    // Assert
    assertEquals("UTF8", zipOutputStream.getEncoding());
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new Zip64ExtendedInformationExtraField(ZipEightByteInteger.ZERO,
        ZipEightByteInteger.ZERO, ZipEightByteInteger.ZERO, ZipLong.CFH_SIG));

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryAddExtraFieldAsiExtraField_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField Instance.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryAddExtraFieldInstance_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(JarMarker.getInstance());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField Instance.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryAddExtraFieldInstance_thenFirstElementIsThree2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(JarMarker.getInstance());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Comment is {@code foo}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryCommentIsFoo_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Comment is {@code foo}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryCommentIsFoo_thenFirstElementIsThree2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Comment is {@code foo}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryCommentIsFoo_thenFirstElementIsThree3() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} ExternalAttributes is forty-two.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryExternalAttributesIsFortyTwo_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setExternalAttributes(42L);

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} InternalAttributes is forty-two.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryInternalAttributesIsFortyTwo_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setInternalAttributes(42);

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Size is {@link Long#MAX_VALUE}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntrySizeIsMax_value_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setSize(Long.MAX_VALUE);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Size is three.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntrySizeIsThree_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setSize(3L);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} UnixMode is three.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipEntryUnixModeIsThree_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setUnixMode(3);

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsAlways() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsAlways2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsAlways3() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsAlways4() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Never}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsNever() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Never);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_thenFirstElementIsThree2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_thenFirstElementIsThree3() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#finish()}.
   * <ul>
   *   <li>Then first element is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#finish()}
   */
  @Test
  public void testFinish_thenFirstElementIsZero() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zipOutputStream.finish();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 0, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#closeEntry()}.
   * <p>
   * Method under test: {@link ZipOutputStream#closeEntry()}
   */
  @Test
  public void testCloseEntry() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.closeEntry();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#closeEntry()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#closeEntry()}
   */
  @Test
  public void testCloseEntry_givenZipEntryAddExtraFieldAsiExtraField_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.closeEntry();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#closeEntry()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Size is three.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#closeEntry()}
   */
  @Test
  public void testCloseEntry_givenZipEntrySizeIsThree_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setSize(3L);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.closeEntry();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#closeEntry()}.
   * <ul>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#closeEntry()}
   */
  @Test
  public void testCloseEntry_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.closeEntry();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#closeEntry()}.
   * <ul>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#closeEntry()}
   */
  @Test
  public void testCloseEntry_thenThrowIOException() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(IOException.class, () -> (new ZipOutputStream(new ByteArrayOutputStream(1))).closeEntry());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setCreateUnicodeExtraFields(null);
    zipOutputStream.putNextEntry(new ZipEntry());
    ZipEntry archiveEntry = new ZipEntry();

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    assertNull(archiveEntry.getExtra());
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
    assertArrayEquals(new byte[]{}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[1];
    assertTrue(zipExtraField instanceof AsiExtraField);
    ZipExtraField zipExtraField2 = extraFields[0];
    assertTrue(zipExtraField2 instanceof Zip64ExtendedInformationExtraField);
    assertEquals(2, extraFields.length);
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField2.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{1, 0}, zipExtraField2.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0},
        ((Zip64ExtendedInformationExtraField) zipExtraField2).getCompressedSize().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry3() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setCreateUnicodeExtraFields(null);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    assertNull(archiveEntry.getExtra());
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
    assertArrayEquals(new byte[]{}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Comment is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenFoo_whenZipEntryCommentIsFoo() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[1];
    assertTrue(zipExtraField instanceof AsiExtraField);
    ZipExtraField zipExtraField2 = extraFields[0];
    assertTrue(zipExtraField2 instanceof Zip64ExtendedInformationExtraField);
    assertEquals(2, extraFields.length);
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField2.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{1, 0}, zipExtraField2.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0},
        ((Zip64ExtendedInformationExtraField) zipExtraField2).getCompressedSize().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given forty-two.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} ExternalAttributes is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenFortyTwo_whenZipEntryExternalAttributesIsFortyTwo() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setExternalAttributes(42L);
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[1];
    assertTrue(zipExtraField instanceof AsiExtraField);
    ZipExtraField zipExtraField2 = extraFields[0];
    assertTrue(zipExtraField2 instanceof Zip64ExtendedInformationExtraField);
    assertEquals(2, extraFields.length);
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField2.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{1, 0}, zipExtraField2.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0},
        ((Zip64ExtendedInformationExtraField) zipExtraField2).getCompressedSize().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given forty-two.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} InternalAttributes is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenFortyTwo_whenZipEntryInternalAttributesIsFortyTwo() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setInternalAttributes(42);
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[1];
    assertTrue(zipExtraField instanceof AsiExtraField);
    ZipExtraField zipExtraField2 = extraFields[0];
    assertTrue(zipExtraField2 instanceof Zip64ExtendedInformationExtraField);
    assertEquals(2, extraFields.length);
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField2.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{1, 0}, zipExtraField2.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0},
        ((Zip64ExtendedInformationExtraField) zipExtraField2).getCompressedSize().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given Instance.</li>
   *   <li>Then second element is Instance.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenInstance_thenSecondElementIsInstance() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    JarMarker ze = JarMarker.getInstance();
    archiveEntry.addExtraField(ze);

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[0];
    assertTrue(zipExtraField instanceof Zip64ExtendedInformationExtraField);
    assertEquals(2, extraFields.length);
    assertSame(ze, extraFields[1]);
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, -54, 0, 0},
        archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, -54, 0, 0},
        archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given {@link Long#MAX_VALUE}.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Size is {@link Long#MAX_VALUE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenMax_value_whenZipEntrySizeIsMax_value() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setSize(Long.MAX_VALUE);
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[1];
    assertTrue(zipExtraField instanceof AsiExtraField);
    ZipExtraField zipExtraField2 = extraFields[0];
    assertTrue(zipExtraField2 instanceof Zip64ExtendedInformationExtraField);
    assertEquals(2, extraFields.length);
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField2.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{1, 0}, zipExtraField2.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0},
        ((Zip64ExtendedInformationExtraField) zipExtraField2).getCompressedSize().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given three.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Method is three.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Method is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenThree_whenZipEntryMethodIsThree_thenZipEntryMethodIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setMethod(3);
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[1];
    assertTrue(zipExtraField instanceof AsiExtraField);
    ZipExtraField zipExtraField2 = extraFields[0];
    assertTrue(zipExtraField2 instanceof Zip64ExtendedInformationExtraField);
    assertEquals(2, extraFields.length);
    assertEquals(3, archiveEntry.getMethod());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given three.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Size is three.</li>
   *   <li>Then first element is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenThree_whenZipEntrySizeIsThree_thenFirstElementIsZero() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setSize(3L);
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 0, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given three.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} UnixMode is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenThree_whenZipEntryUnixModeIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setUnixMode(3);
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[1];
    assertTrue(zipExtraField instanceof AsiExtraField);
    ZipExtraField zipExtraField2 = extraFields[0];
    assertTrue(zipExtraField2 instanceof Zip64ExtendedInformationExtraField);
    assertEquals(2, extraFields.length);
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField2.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{1, 0}, zipExtraField2.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0},
        ((Zip64ExtendedInformationExtraField) zipExtraField2).getCompressedSize().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField2.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'n', 'u', 14, 0, 'v', 'h',
        -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given {@link Zip64ExtendedInformationExtraField#Zip64ExtendedInformationExtraField()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenZip64ExtendedInformationExtraField() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new Zip64ExtendedInformationExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[0];
    assertTrue(zipExtraField instanceof Zip64ExtendedInformationExtraField);
    assertEquals(1, extraFields.length);
    ZipEightByteInteger compressedSize = ((Zip64ExtendedInformationExtraField) zipExtraField).getCompressedSize();
    assertArrayEquals(new byte[]{0}, compressedSize.getValue().toByteArray());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0}, compressedSize.getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Comment is {@code foo}.</li>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_givenZipEntryCommentIsFoo_whenZipEntry_thenZipEntryExtraIsNull() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    ZipEntry archiveEntry2 = new ZipEntry();

    // Act
    zipOutputStream.putNextEntry(archiveEntry2);

    // Assert
    assertNull(archiveEntry2.getExtra());
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
    assertArrayEquals(new byte[]{}, archiveEntry2.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Then array length is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_thenArrayLengthIsOne() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());
    ZipEntry archiveEntry = new ZipEntry();

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[0];
    assertTrue(zipExtraField instanceof Zip64ExtendedInformationExtraField);
    assertEquals(1, extraFields.length);
    ZipEightByteInteger compressedSize = ((Zip64ExtendedInformationExtraField) zipExtraField).getCompressedSize();
    assertArrayEquals(new byte[]{0}, compressedSize.getValue().toByteArray());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{1, 0}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0}, compressedSize.getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Then array length is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_thenArrayLengthIsOne2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    ZipEntry archiveEntry = new ZipEntry();

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    ZipExtraField[] extraFields = archiveEntry.getExtraFields();
    ZipExtraField zipExtraField = extraFields[0];
    assertTrue(zipExtraField instanceof Zip64ExtendedInformationExtraField);
    assertEquals(1, extraFields.length);
    ZipEightByteInteger compressedSize = ((Zip64ExtendedInformationExtraField) zipExtraField).getCompressedSize();
    assertArrayEquals(new byte[]{0}, compressedSize.getValue().toByteArray());
    assertArrayEquals(new byte[]{16, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{1, 0}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0}, compressedSize.getBytes());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, zipExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, archiveEntry.getExtra());
    assertArrayEquals(new byte[]{1, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Then first element is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_thenFirstElementIsZero() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zipOutputStream.putNextEntry(new ZipEntry());

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 0, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Then first element is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_thenFirstElementIsZero2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 0, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Then throw {@link ZipException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_thenThrowZipException() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setMethod(0);

    // Act and Assert
    assertThrows(ZipException.class, () -> zipOutputStream.putNextEntry(new ZipEntry()));
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>Then {@link ZipEntry#ZipEntry()} Method is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_thenZipEntryMethodIsOne() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setMethod(1);
    ZipEntry archiveEntry = new ZipEntry();

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    assertEquals(1, archiveEntry.getMethod());
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_whenZipEntry_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.putNextEntry(new ZipEntry());

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#putNextEntry(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then {@link ZipEntry#ZipEntry()} Extra is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#putNextEntry(ZipEntry)}
   */
  @Test
  public void testPutNextEntry_whenZipEntry_thenZipEntryExtraIsNull() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    ZipEntry archiveEntry = new ZipEntry();

    // Act
    zipOutputStream.putNextEntry(archiveEntry);

    // Assert
    assertNull(archiveEntry.getExtra());
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
    assertArrayEquals(new byte[]{}, archiveEntry.getCentralDirectoryExtra());
  }

  /**
   * Test {@link ZipOutputStream#setLevel(int)}.
   * <ul>
   *   <li>When minus two.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#setLevel(int)}
   */
  @Test
  public void testSetLevel_whenMinusTwo_thenThrowIllegalArgumentException() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> (new ZipOutputStream(new ByteArrayOutputStream(1))).setLevel(-2));
  }

  /**
   * Test {@link ZipOutputStream#setLevel(int)}.
   * <ul>
   *   <li>When ten.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#setLevel(int)}
   */
  @Test
  public void testSetLevel_whenTen_thenThrowIllegalArgumentException() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> (new ZipOutputStream(new ByteArrayOutputStream(1))).setLevel(10));
  }

  /**
   * Test {@link ZipOutputStream#canWriteEntryData(ZipEntry)}.
   * <ul>
   *   <li>Given eight.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Method is eight.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#canWriteEntryData(ZipEntry)}
   */
  @Test
  public void testCanWriteEntryData_givenEight_whenZipEntryMethodIsEight_thenReturnTrue() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    GeneralPurposeBit b = new GeneralPurposeBit();
    b.useEncryption(false);

    ZipEntry ae = new ZipEntry();
    ae.setMethod(8);
    ae.setGeneralPurposeBit(b);

    // Act and Assert
    assertTrue(zipOutputStream.canWriteEntryData(ae));
  }

  /**
   * Test {@link ZipOutputStream#canWriteEntryData(ZipEntry)}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor) useEncryption {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#canWriteEntryData(ZipEntry)}
   */
  @Test
  public void testCanWriteEntryData_givenGeneralPurposeBitUseEncryptionTrue_thenReturnFalse() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    GeneralPurposeBit b = new GeneralPurposeBit();
    b.useEncryption(true);

    ZipEntry ae = new ZipEntry();
    ae.setMethod(0);
    ae.setGeneralPurposeBit(b);

    // Act and Assert
    assertFalse(zipOutputStream.canWriteEntryData(ae));
  }

  /**
   * Test {@link ZipOutputStream#canWriteEntryData(ZipEntry)}.
   * <ul>
   *   <li>Given zero.</li>
   *   <li>When {@link ZipEntry#ZipEntry()} Method is zero.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#canWriteEntryData(ZipEntry)}
   */
  @Test
  public void testCanWriteEntryData_givenZero_whenZipEntryMethodIsZero_thenReturnTrue() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    GeneralPurposeBit b = new GeneralPurposeBit();
    b.useEncryption(false);

    ZipEntry ae = new ZipEntry();
    ae.setMethod(0);
    ae.setGeneralPurposeBit(b);

    // Act and Assert
    assertTrue(zipOutputStream.canWriteEntryData(ae));
  }

  /**
   * Test {@link ZipOutputStream#canWriteEntryData(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#canWriteEntryData(ZipEntry)}
   */
  @Test
  public void testCanWriteEntryData_whenZipEntry_thenReturnFalse() {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act and Assert
    assertFalse(zipOutputStream.canWriteEntryData(new ZipEntry()));
  }

  /**
   * Test {@link ZipOutputStream#write(byte[], int, int)} with {@code b}, {@code offset}, {@code length}.
   * <p>
   * Method under test: {@link ZipOutputStream#write(byte[], int, int)}
   */
  @Test
  public void testWriteWithBOffsetLength() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.write("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Assert
    Deflater deflater = zipOutputStream.def;
    assertEquals(3, deflater.getTotalIn());
    assertEquals(3L, deflater.getBytesRead());
  }

  /**
   * Test {@link ZipOutputStream#write(byte[], int, int)} with {@code b}, {@code offset}, {@code length}.
   * <p>
   * Method under test: {@link ZipOutputStream#write(byte[], int, int)}
   */
  @Test
  public void testWriteWithBOffsetLength2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.write("AXAXAXAX".getBytes("UTF-8"), 2, 0);

    // Assert that nothing has changed
    Deflater deflater = zipOutputStream.def;
    assertEquals(0, deflater.getTotalIn());
    assertEquals(0L, deflater.getBytesRead());
  }

  /**
   * Test {@link ZipOutputStream#write(byte[], int, int)} with {@code b}, {@code offset}, {@code length}.
   * <ul>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#write(byte[], int, int)}
   */
  @Test
  public void testWriteWithBOffsetLength_thenThrowIllegalStateException() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act and Assert
    assertThrows(IllegalStateException.class, () -> zipOutputStream.write("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }

  /**
   * Test {@link ZipOutputStream#write(int)} with {@code b}.
   * <ul>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#write(int)}
   */
  @Test
  public void testWriteWithB_thenThrowIllegalStateException() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(IllegalStateException.class,
        () -> (new ZipOutputStream(new ByteArrayOutputStream(1))).write(19088743));
  }

  /**
   * Test {@link ZipOutputStream#write(int)} with {@code b}.
   * <ul>
   *   <li>Then {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} {@link ZipOutputStream#def} TotalIn is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#write(int)}
   */
  @Test
  public void testWriteWithB_thenZipOutputStreamWithOutIsByteArrayOutputStreamDefTotalInIsOne() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.write(19088743);

    // Assert
    Deflater deflater = zipOutputStream.def;
    assertEquals(1, deflater.getTotalIn());
    assertEquals(1L, deflater.getBytesRead());
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new Zip64ExtendedInformationExtraField(ZipEightByteInteger.ZERO,
        ZipEightByteInteger.ZERO, ZipEightByteInteger.ZERO, ZipLong.CFH_SIG));

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryAddExtraFieldAsiExtraField_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryAddExtraFieldAsiExtraField_thenFirstElementIsThree2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField {@link AsiExtraField} (default constructor).</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryAddExtraFieldAsiExtraField_thenFirstElementIsThree3() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipEntry archiveEntry2 = new ZipEntry();
    archiveEntry2.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry2);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField Instance.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryAddExtraFieldInstance_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(JarMarker.getInstance());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} addExtraField Instance.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryAddExtraFieldInstance_thenFirstElementIsThree2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(JarMarker.getInstance());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Comment is {@code foo}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryCommentIsFoo_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Comment is {@code foo}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryCommentIsFoo_thenFirstElementIsThree2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Comment is {@code foo}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryCommentIsFoo_thenFirstElementIsThree3() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setComment("foo");
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} ExternalAttributes is forty-two.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryExternalAttributesIsFortyTwo_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setExternalAttributes(42L);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} InternalAttributes is forty-two.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryInternalAttributesIsFortyTwo_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setInternalAttributes(42);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Size is {@link Long#MAX_VALUE}.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntrySizeIsMax_value_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setSize(Long.MAX_VALUE);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} Size is three.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntrySizeIsThree_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setSize(3L);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry()} UnixMode is three.</li>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipEntryUnixModeIsThree_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.setUnixMode(3);
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(archiveEntry);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsAlways() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsAlways2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsAlways3() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsAlways4() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry archiveEntry = new ZipEntry();
    archiveEntry.addExtraField(new AsiExtraField());

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.setUseZip64(Zip64Mode.Always);
    zipOutputStream.putNextEntry(archiveEntry);

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Given {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} UseZip64 is {@code Never}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_givenZipOutputStreamWithOutIsByteArrayOutputStreamUseZip64IsNever() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.setUseZip64(Zip64Mode.Never);
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_thenFirstElementIsThree() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Then first element is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_thenFirstElementIsThree2() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));
    zipOutputStream.putNextEntry(new ZipEntry());
    zipOutputStream.putNextEntry(new ZipEntry());

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 3, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#close()}.
   * <ul>
   *   <li>Then first element is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#close()}
   */
  @Test
  public void testClose_thenFirstElementIsZero() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    ZipOutputStream zipOutputStream = new ZipOutputStream(new ByteArrayOutputStream(1));

    // Act
    zipOutputStream.close();

    // Assert that nothing has changed
    byte[] byteArray = zipOutputStream.buf;
    assertEquals((byte) 0, byteArray[0]);
    assertEquals(512, byteArray.length);
  }

  /**
   * Test {@link ZipOutputStream#writeCentralDirectoryEnd()}.
   * <ul>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#writeCentralDirectoryEnd()}
   */
  @Test
  public void testWriteCentralDirectoryEnd_thenThrowIllegalStateException() throws IOException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(IllegalStateException.class,
        () -> (new ZipOutputStream(new ZipOutputStream(new ByteArrayOutputStream(1)))).writeCentralDirectoryEnd());
  }

  /**
   * Test {@link ZipOutputStream#toDosTime(long)} with {@code t}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return array of {@code byte} with zero and {@code !}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#toDosTime(long)}
   */
  @Test
  public void testToDosTimeWithT_whenOne_thenReturnArrayOfByteWithZeroAndExclamationMark() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{0, '!', 0, 0}, ZipOutputStream.toDosTime(1L));
  }

  /**
   * Test {@link ZipOutputStream#toDosTime(Date)} with {@code time}.
   * <ul>
   *   <li>Then return Value is {@code 8448}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#toDosTime(Date)}
   */
  @Test
  public void testToDosTimeWithTime_thenReturnValueIs8448() {
    // Arrange and Act
    ZipLong actualToDosTimeResult = ZipOutputStream
        .toDosTime(Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant()));

    // Assert
    assertEquals(8448L, actualToDosTimeResult.getValue());
    assertArrayEquals(new byte[]{0, '!', 0, 0}, actualToDosTimeResult.getBytes());
  }

  /**
   * Test {@link ZipOutputStream#getBytes(String)}.
   * <ul>
   *   <li>Then return {@code Name} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#getBytes(String)}
   */
  @Test
  public void testGetBytes_thenReturnNameBytesIsUtf8() throws UnsupportedEncodingException, ZipException {
    // Arrange
    Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    byte[] actualBytes = (new ZipOutputStream(new ByteArrayOutputStream(1))).getBytes("Name");

    // Assert
    assertArrayEquals("Name".getBytes("UTF-8"), actualBytes);
  }

  /**
   * Test {@link ZipOutputStream#adjustToLong(int)}.
   * <ul>
   *   <li>When {@link ZipOutputStream#DEFAULT_COMPRESSION}.</li>
   *   <li>Then return {@code 4294967295}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#adjustToLong(int)}
   */
  @Test
  public void testAdjustToLong_whenDefault_compression_thenReturn4294967295() {
    // Arrange, Act and Assert
    assertEquals(4294967295L, ZipOutputStream.adjustToLong(ZipOutputStream.DEFAULT_COMPRESSION));
  }

  /**
   * Test {@link ZipOutputStream#adjustToLong(int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipOutputStream#adjustToLong(int)}
   */
  @Test
  public void testAdjustToLong_whenOne_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1L, ZipOutputStream.adjustToLong(1));
  }
}
