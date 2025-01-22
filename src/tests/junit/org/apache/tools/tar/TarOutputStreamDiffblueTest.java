package org.apache.tools.tar;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

public class TarOutputStreamDiffblueTest {
  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream)}.
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream)}
   */
  @Test
  public void testNewTarOutputStream() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1));

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code ASCII}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenAscii_thenReturnCurrNameIsNull() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, 3, "ASCII");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertEquals(3, actualTarOutputStream.getRecordSize());
    assertFalse(actualTarOutputStream.debug);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.assemBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.recordBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}.
   * <ul>
   *   <li>When {@code ASCII}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenAscii_thenReturnCurrNameIsNull2() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, "ASCII");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, String)}.
   * <ul>
   *   <li>When {@code ASCII}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, String)}
   */
  @Test
  public void testNewTarOutputStream_whenAscii_thenReturnCurrNameIsNull3() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), "ASCII");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, int)}.
   * <ul>
   *   <li>When {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, int)}
   */
  @Test
  public void testNewTarOutputStream_whenByteArrayOutputStreamWithOne_thenReturnCurrNameIsNull() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, 3);

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertEquals(3, actualTarOutputStream.getRecordSize());
    assertFalse(actualTarOutputStream.debug);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.assemBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.recordBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code Encoding}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenEncoding_thenReturnCurrNameIsNull() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, 3, "Encoding");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertEquals(3, actualTarOutputStream.getRecordSize());
    assertFalse(actualTarOutputStream.debug);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.assemBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.recordBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}.
   * <ul>
   *   <li>When {@code Encoding}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenEncoding_thenReturnCurrNameIsNull2() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, "Encoding");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, String)}.
   * <ul>
   *   <li>When {@code Encoding}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, String)}
   */
  @Test
  public void testNewTarOutputStream_whenEncoding_thenReturnCurrNameIsNull3() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), "Encoding");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenNull_thenReturnCurrNameIsNull() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, 3, null);

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertEquals(3, actualTarOutputStream.getRecordSize());
    assertFalse(actualTarOutputStream.debug);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.assemBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.recordBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenNull_thenReturnCurrNameIsNull2() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, null);

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, String)}
   */
  @Test
  public void testNewTarOutputStream_whenNull_thenReturnCurrNameIsNull3() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), null);

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int)}
   */
  @Test
  public void testNewTarOutputStream_whenThree_thenReturnCurrNameIsNull() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3);

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code UTF-8}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenUtf8_thenReturnCurrNameIsNull() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, 3, "UTF-8");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertEquals(3, actualTarOutputStream.getRecordSize());
    assertFalse(actualTarOutputStream.debug);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.assemBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.recordBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenUtf8_thenReturnCurrNameIsNull2() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, 3, "UTF8");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertEquals(3, actualTarOutputStream.getRecordSize());
    assertFalse(actualTarOutputStream.debug);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.assemBuf);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM, TarConstants.LF_OLDNORM},
        actualTarOutputStream.recordBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}.
   * <ul>
   *   <li>When {@code UTF-8}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenUtf8_thenReturnCurrNameIsNull3() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, "UTF-8");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, int, String)}
   */
  @Test
  public void testNewTarOutputStream_whenUtf8_thenReturnCurrNameIsNull4() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3, "UTF8");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, String)}.
   * <ul>
   *   <li>When {@code UTF-8}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, String)}
   */
  @Test
  public void testNewTarOutputStream_whenUtf8_thenReturnCurrNameIsNull5() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), "UTF-8");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#TarOutputStream(OutputStream, String)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then return {@link TarOutputStream#currName} is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#TarOutputStream(OutputStream, String)}
   */
  @Test
  public void testNewTarOutputStream_whenUtf8_thenReturnCurrNameIsNull6() {
    // Arrange and Act
    TarOutputStream actualTarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), "UTF8");

    // Assert
    assertNull(actualTarOutputStream.currName);
    assertEquals(0, actualTarOutputStream.assemLen);
    assertEquals(0, actualTarOutputStream.longFileMode);
    assertEquals(0L, actualTarOutputStream.currBytes);
    assertEquals(0L, actualTarOutputStream.currSize);
    assertFalse(actualTarOutputStream.debug);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.getRecordSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.assemBuf.length);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarOutputStream.recordBuf.length);
    assertArrayEquals(new byte[]{TarConstants.LF_OLDNORM}, actualTarOutputStream.oneBuf);
  }

  /**
   * Test {@link TarOutputStream#finish()}.
   * <p>
   * Method under test: {@link TarOutputStream#finish()}
   */
  @Test
  public void testFinish() throws IOException {
    // Arrange
    TarOutputStream tarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), TarBuffer.DEFAULT_RCDSIZE);

    // Act
    tarOutputStream.finish();

    // Assert
    assertEquals(2, tarOutputStream.buffer.getCurrentBlockNum());
  }

  /**
   * Test {@link TarOutputStream#finish()}.
   * <ul>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#finish()}
   */
  @Test
  public void testFinish_thenThrowIOException() throws IOException {
    // Arrange
    TarOutputStream tarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), TarBuffer.DEFAULT_RCDSIZE);
    tarOutputStream.putNextEntry(new TarEntry("Name"));

    // Act and Assert
    assertThrows(IOException.class, () -> tarOutputStream.finish());
  }

  /**
   * Test {@link TarOutputStream#close()}.
   * <p>
   * Method under test: {@link TarOutputStream#close()}
   */
  @Test
  public void testClose() throws IOException {
    // Arrange
    TarOutputStream tarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), TarBuffer.DEFAULT_RCDSIZE);

    // Act
    tarOutputStream.close();

    // Assert
    assertEquals(2, tarOutputStream.buffer.getCurrentBlockNum());
  }

  /**
   * Test {@link TarOutputStream#getRecordSize()}.
   * <p>
   * Method under test: {@link TarOutputStream#getRecordSize()}
   */
  @Test
  public void testGetRecordSize() {
    // Arrange, Act and Assert
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, (new TarOutputStream(new ByteArrayOutputStream(1), 3)).getRecordSize());
  }

  /**
   * Test {@link TarOutputStream#putNextEntry(TarEntry)}.
   * <ul>
   *   <li>Then array length is {@link TarBuffer#DEFAULT_RCDSIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#putNextEntry(TarEntry)}
   */
  @Test
  public void testPutNextEntry_thenArrayLengthIsDefault_rcdsize() throws IOException {
    // Arrange
    TarOutputStream tarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 2097151);

    // Act
    tarOutputStream.putNextEntry(new TarEntry("Name"));

    // Assert
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, tarOutputStream.recordBuf.length);
  }

  /**
   * Test {@link TarOutputStream#closeEntry()}.
   * <p>
   * Method under test: {@link TarOutputStream#closeEntry()}
   */
  @Test
  public void testCloseEntry() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> (new TarOutputStream(new ByteArrayOutputStream(1), 3)).closeEntry());
  }

  /**
   * Test {@link TarOutputStream#closeEntry()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with name is {@code No current entry to close} Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#closeEntry()}
   */
  @Test
  public void testCloseEntry_givenTarEntryWithNameIsNoCurrentEntryToCloseSizeIsThree() throws IOException {
    // Arrange
    TarEntry entry = new TarEntry("No current entry to close");
    entry.setSize(3L);

    TarOutputStream tarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), "UTF-8");
    tarOutputStream.putNextEntry(entry);

    // Act and Assert
    assertThrows(IOException.class, () -> tarOutputStream.closeEntry());
  }

  /**
   * Test {@link TarOutputStream#write(int)} with {@code b}.
   * <p>
   * Method under test: {@link TarOutputStream#write(int)}
   */
  @Test
  public void testWriteWithB() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> (new TarOutputStream(new ByteArrayOutputStream(1), 3)).write(19088743));
  }

  /**
   * Test {@link TarOutputStream#write(byte[], int, int)} with {@code wBuf}, {@code wOffset}, {@code numToWrite}.
   * <ul>
   *   <li>When ten.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#write(byte[], int, int)}
   */
  @Test
  public void testWriteWithWBufWOffsetNumToWrite_whenTen_thenThrowIOException() throws IOException {
    // Arrange
    TarOutputStream tarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3);

    // Act and Assert
    assertThrows(IOException.class, () -> tarOutputStream.write("AXAXAXAX".getBytes("UTF-8"), 19088743, 10));
  }

  /**
   * Test {@link TarOutputStream#write(byte[])} with {@code wBuf}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarOutputStream#write(byte[])}
   */
  @Test
  public void testWriteWithWBuf_whenAxaxaxaxBytesIsUtf8_thenThrowIOException() throws IOException {
    // Arrange
    TarOutputStream tarOutputStream = new TarOutputStream(new ByteArrayOutputStream(1), 3);

    // Act and Assert
    assertThrows(IOException.class, () -> tarOutputStream.write("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link TarOutputStream#writePaxHeaders(TarEntry, String, Map)}.
   * <p>
   * Method under test: {@link TarOutputStream#writePaxHeaders(TarEntry, String, Map)}
   */
  @Test
  public void testWritePaxHeaders() throws IOException {
    // Arrange
    TarOutputStream tarOutputStream = new TarOutputStream(new ByteArrayOutputStream(10), "UTF-8");
    TarEntry entry = new TarEntry("Name");

    HashMap<String, String> headers = new HashMap<>();
    headers.put("Delivered-To", "alice.liddell@example.org");

    // Act
    tarOutputStream.writePaxHeaders(entry, "Entry Name", headers);

    // Assert
    assertEquals("./PaxHeaders.X/Entry Name", tarOutputStream.currName);
    assertEquals(42L, tarOutputStream.currBytes);
    assertEquals(42L, tarOutputStream.currSize);
  }
}
