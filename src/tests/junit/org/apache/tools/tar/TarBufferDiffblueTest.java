package org.apache.tools.tar;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class TarBufferDiffblueTest {
  /**
   * Test {@link TarBuffer#TarBuffer(OutputStream)}.
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(OutputStream)}
   */
  @Test
  public void testNewTarBuffer() {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer(new ByteArrayOutputStream(1));

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentRecordNum());
    assertEquals(0, actualTarBuffer.getCurrentBlockNum());
    assertEquals(TarBuffer.DEFAULT_BLKSIZE, actualTarBuffer.getBlockSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#TarBuffer(InputStream)}.
   * <ul>
   *   <li>Then return CurrentBlockNum is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(InputStream)}
   */
  @Test
  public void testNewTarBuffer_thenReturnCurrentBlockNumIsMinusOne() throws UnsupportedEncodingException {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentBlockNum());
    assertEquals(19, actualTarBuffer.getCurrentRecordNum());
    assertEquals(TarBuffer.DEFAULT_BLKSIZE, actualTarBuffer.getBlockSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#TarBuffer(InputStream, int)}.
   * <ul>
   *   <li>Then return CurrentBlockNum is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(InputStream, int)}
   */
  @Test
  public void testNewTarBuffer_thenReturnCurrentBlockNumIsMinusOne2() throws UnsupportedEncodingException {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3);

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentBlockNum());
    assertEquals(-1, actualTarBuffer.getCurrentRecordNum());
    assertEquals(3, actualTarBuffer.getBlockSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#TarBuffer(InputStream, int, int)}.
   * <ul>
   *   <li>Then return CurrentBlockNum is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(InputStream, int, int)}
   */
  @Test
  public void testNewTarBuffer_thenReturnCurrentBlockNumIsMinusOne3() throws UnsupportedEncodingException {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, 3);

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentBlockNum());
    assertEquals(0, actualTarBuffer.getCurrentRecordNum());
    assertEquals(3, actualTarBuffer.getBlockSize());
    assertEquals(3, actualTarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#TarBuffer(OutputStream, int, int)}.
   * <ul>
   *   <li>Then return CurrentRecordNum is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(OutputStream, int, int)}
   */
  @Test
  public void testNewTarBuffer_thenReturnCurrentRecordNumIsMinusOne() {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer(new ByteArrayOutputStream(1), 3, 3);

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentRecordNum());
    assertEquals(0, actualTarBuffer.getCurrentBlockNum());
    assertEquals(3, actualTarBuffer.getBlockSize());
    assertEquals(3, actualTarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#TarBuffer(InputStream, int)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return CurrentBlockNum is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(InputStream, int)}
   */
  @Test
  public void testNewTarBuffer_whenNull_thenReturnCurrentBlockNumIsZero() {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer((InputStream) null, 3);

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentRecordNum());
    assertEquals(0, actualTarBuffer.getCurrentBlockNum());
    assertEquals(3, actualTarBuffer.getBlockSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#TarBuffer(InputStream)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return CurrentRecordNum is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(InputStream)}
   */
  @Test
  public void testNewTarBuffer_whenNull_thenReturnCurrentRecordNumIsMinusOne() {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer((InputStream) null);

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentRecordNum());
    assertEquals(0, actualTarBuffer.getCurrentBlockNum());
    assertEquals(TarBuffer.DEFAULT_BLKSIZE, actualTarBuffer.getBlockSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#TarBuffer(InputStream, int, int)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return CurrentRecordNum is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(InputStream, int, int)}
   */
  @Test
  public void testNewTarBuffer_whenNull_thenReturnCurrentRecordNumIsMinusOne2() {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer((InputStream) null, 3, 3);

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentRecordNum());
    assertEquals(0, actualTarBuffer.getCurrentBlockNum());
    assertEquals(3, actualTarBuffer.getBlockSize());
    assertEquals(3, actualTarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#TarBuffer(OutputStream, int)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then return CurrentRecordNum is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#TarBuffer(OutputStream, int)}
   */
  @Test
  public void testNewTarBuffer_whenThree_thenReturnCurrentRecordNumIsMinusOne() {
    // Arrange and Act
    TarBuffer actualTarBuffer = new TarBuffer(new ByteArrayOutputStream(1), 3);

    // Assert
    assertEquals(-1, actualTarBuffer.getCurrentRecordNum());
    assertEquals(0, actualTarBuffer.getCurrentBlockNum());
    assertEquals(3, actualTarBuffer.getBlockSize());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualTarBuffer.getRecordSize());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TarBuffer#setDebug(boolean)}
   *   <li>{@link TarBuffer#getBlockSize()}
   *   <li>{@link TarBuffer#getCurrentBlockNum()}
   *   <li>{@link TarBuffer#getRecordSize()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws UnsupportedEncodingException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act
    tarBuffer.setDebug(true);
    int actualBlockSize = tarBuffer.getBlockSize();
    int actualCurrentBlockNum = tarBuffer.getCurrentBlockNum();

    // Assert
    assertEquals(-1, actualCurrentBlockNum);
    assertEquals(TarBuffer.DEFAULT_BLKSIZE, actualBlockSize);
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, tarBuffer.getRecordSize());
  }

  /**
   * Test {@link TarBuffer#isEOFRecord(byte[])}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#isEOFRecord(byte[])}
   */
  @Test
  public void testIsEOFRecord_whenA_thenReturnFalse() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertFalse((new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))))
        .isEOFRecord(new byte[]{TarConstants.LF_OLDNORM, 'X', 'A', 'X', 'A', 'X', 'A', 'X'}));
  }

  /**
   * Test {@link TarBuffer#isEOFRecord(byte[])}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#isEOFRecord(byte[])}
   */
  @Test
  public void testIsEOFRecord_whenAxaxaxaxBytesIsUtf8_thenReturnFalse() throws UnsupportedEncodingException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act and Assert
    assertFalse(tarBuffer.isEOFRecord("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link TarBuffer#skipRecord()}.
   * <p>
   * Method under test: {@link TarBuffer#skipRecord()}
   */
  @Test
  public void testSkipRecord() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act
    tarBuffer.skipRecord();

    // Assert
    assertEquals(0, tarBuffer.getCurrentBlockNum());
    assertEquals(0, tarBuffer.getCurrentRecordNum());
  }

  /**
   * Test {@link TarBuffer#skipRecord()}.
   * <p>
   * Method under test: {@link TarBuffer#skipRecord()}
   */
  @Test
  public void testSkipRecord2() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream(new byte[]{}));

    // Act
    tarBuffer.skipRecord();

    // Assert
    assertEquals(-1, tarBuffer.getCurrentBlockNum());
    assertEquals(-1, tarBuffer.getCurrentRecordNum());
  }

  /**
   * Test {@link TarBuffer#skipRecord()}.
   * <p>
   * Method under test: {@link TarBuffer#skipRecord()}
   */
  @Test
  public void testSkipRecord3() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream(new byte[]{'A', 20, 'A', 20, 'A', 20, 'A', 20}), 3);

    // Act
    tarBuffer.skipRecord();

    // Assert
    assertEquals(0, tarBuffer.getCurrentBlockNum());
    assertEquals(0, tarBuffer.getCurrentRecordNum());
  }

  /**
   * Test {@link TarBuffer#skipRecord()}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} Debug is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#skipRecord()}
   */
  @Test
  public void testSkipRecord_givenTarBufferWithInStreamIsByteArrayInputStreamDebugIsTrue() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));
    tarBuffer.setDebug(true);

    // Act
    tarBuffer.skipRecord();

    // Assert
    assertEquals(0, tarBuffer.getCurrentBlockNum());
    assertEquals(0, tarBuffer.getCurrentRecordNum());
  }

  /**
   * Test {@link TarBuffer#skipRecord()}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@code null}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#skipRecord()}
   */
  @Test
  public void testSkipRecord_givenTarBufferWithInStreamIsNull_thenThrowIOException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> (new TarBuffer((InputStream) null)).skipRecord());
  }

  /**
   * Test {@link TarBuffer#readRecord()}.
   * <p>
   * Method under test: {@link TarBuffer#readRecord()}
   */
  @Test
  public void testReadRecord() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act
    byte[] actualReadRecordResult = tarBuffer.readRecord();

    // Assert
    assertEquals(0, tarBuffer.getCurrentBlockNum());
    assertEquals(0, tarBuffer.getCurrentRecordNum());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualReadRecordResult.length);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[10]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[11]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[13]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[14]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[15]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[17]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[18]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[19]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[20]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[21]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[22]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[23]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[24]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[487]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[488]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[489]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[490]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[491]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[492]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[493]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[494]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[495]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[496]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[497]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[498]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[499]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[500]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[501]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[502]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[503]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[505]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[506]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[507]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[508]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[509]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[510]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[511]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[8]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[9]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[Short.SIZE]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[TarConstants.ATIMELEN_GNU]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[TarConstants.SPARSELEN_GNU_SPARSE]);
    assertEquals('A', actualReadRecordResult[0]);
    assertEquals('A', actualReadRecordResult[2]);
    assertEquals('A', actualReadRecordResult[4]);
    assertEquals('A', actualReadRecordResult[6]);
    assertEquals('X', actualReadRecordResult[1]);
    assertEquals('X', actualReadRecordResult[3]);
    assertEquals('X', actualReadRecordResult[5]);
    assertEquals('X', actualReadRecordResult[7]);
  }

  /**
   * Test {@link TarBuffer#readRecord()}.
   * <ul>
   *   <li>Given {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with empty array of {@code byte}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#readRecord()}
   */
  @Test
  public void testReadRecord_givenByteArrayInputStreamWithEmptyArrayOfByte_thenReturnNull() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream(new byte[]{}));

    // Act and Assert
    assertNull(tarBuffer.readRecord());
    assertEquals(-1, tarBuffer.getCurrentBlockNum());
    assertEquals(-1, tarBuffer.getCurrentRecordNum());
  }

  /**
   * Test {@link TarBuffer#readRecord()}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with twenty.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#readRecord()}
   */
  @Test
  public void testReadRecord_givenByteArrayOutputStreamWithTwenty_thenThrowIOException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> (new TarBuffer(new ByteArrayOutputStream(20))).readRecord());
  }

  /**
   * Test {@link TarBuffer#readRecord()}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} Debug is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#readRecord()}
   */
  @Test
  public void testReadRecord_givenTarBufferWithInStreamIsByteArrayInputStreamDebugIsTrue() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));
    tarBuffer.setDebug(true);

    // Act
    byte[] actualReadRecordResult = tarBuffer.readRecord();

    // Assert
    assertEquals(0, tarBuffer.getCurrentBlockNum());
    assertEquals(0, tarBuffer.getCurrentRecordNum());
    assertEquals(TarBuffer.DEFAULT_RCDSIZE, actualReadRecordResult.length);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[10]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[11]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[13]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[14]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[15]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[17]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[18]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[19]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[20]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[21]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[22]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[23]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[24]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[487]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[488]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[489]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[490]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[491]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[492]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[493]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[494]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[495]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[496]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[497]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[498]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[499]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[500]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[501]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[502]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[503]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[505]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[506]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[507]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[508]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[509]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[510]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[511]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[8]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[9]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[Short.SIZE]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[TarConstants.ATIMELEN_GNU]);
    assertEquals(TarConstants.LF_OLDNORM, actualReadRecordResult[TarConstants.SPARSELEN_GNU_SPARSE]);
    assertEquals('A', actualReadRecordResult[0]);
    assertEquals('A', actualReadRecordResult[2]);
    assertEquals('A', actualReadRecordResult[4]);
    assertEquals('A', actualReadRecordResult[6]);
    assertEquals('X', actualReadRecordResult[1]);
    assertEquals('X', actualReadRecordResult[3]);
    assertEquals('X', actualReadRecordResult[5]);
    assertEquals('X', actualReadRecordResult[7]);
  }

  /**
   * Test {@link TarBuffer#readRecord()}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@code null}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#readRecord()}
   */
  @Test
  public void testReadRecord_givenTarBufferWithInStreamIsNull_thenThrowIOException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> (new TarBuffer((InputStream) null)).readRecord());
  }

  /**
   * Test {@link TarBuffer#getCurrentRecordNum()}.
   * <p>
   * Method under test: {@link TarBuffer#getCurrentRecordNum()}
   */
  @Test
  public void testGetCurrentRecordNum() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(19, (new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")))).getCurrentRecordNum());
  }

  /**
   * Test {@link TarBuffer#writeRecord(byte[], int)} with {@code buf}, {@code offset}.
   * <p>
   * Method under test: {@link TarBuffer#writeRecord(byte[], int)}
   */
  @Test
  public void testWriteRecordWithBufOffset() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));
    tarBuffer.setDebug(true);

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.writeRecord("AXAXAXAX".getBytes("UTF-8"), 2));
  }

  /**
   * Test {@link TarBuffer#writeRecord(byte[], int)} with {@code buf}, {@code offset}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#writeRecord(byte[], int)}
   */
  @Test
  public void testWriteRecordWithBufOffset_givenByteArrayOutputStreamWithOne() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayOutputStream(1));

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.writeRecord("AXAXAXAX".getBytes("UTF-8"), 2));
  }

  /**
   * Test {@link TarBuffer#writeRecord(byte[], int)} with {@code buf}, {@code offset}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@link ByteArrayInputStream#ByteArrayInputStream(byte[])}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#writeRecord(byte[], int)}
   */
  @Test
  public void testWriteRecordWithBufOffset_givenTarBufferWithInStreamIsByteArrayInputStream() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.writeRecord("AXAXAXAX".getBytes("UTF-8"), 2));
  }

  /**
   * Test {@link TarBuffer#writeRecord(byte[], int)} with {@code buf}, {@code offset}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#writeRecord(byte[], int)}
   */
  @Test
  public void testWriteRecordWithBufOffset_givenTarBufferWithInStreamIsNull() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer((InputStream) null);

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.writeRecord("AXAXAXAX".getBytes("UTF-8"), 2));
  }

  /**
   * Test {@link TarBuffer#writeRecord(byte[])} with {@code record}.
   * <p>
   * Method under test: {@link TarBuffer#writeRecord(byte[])}
   */
  @Test
  public void testWriteRecordWithRecord() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));
    tarBuffer.setDebug(true);

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.writeRecord("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link TarBuffer#writeRecord(byte[])} with {@code record}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#writeRecord(byte[])}
   */
  @Test
  public void testWriteRecordWithRecord_givenByteArrayOutputStreamWithOne_thenThrowIOException() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayOutputStream(1));

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.writeRecord("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link TarBuffer#writeRecord(byte[])} with {@code record}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@link ByteArrayInputStream#ByteArrayInputStream(byte[])}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#writeRecord(byte[])}
   */
  @Test
  public void testWriteRecordWithRecord_givenTarBufferWithInStreamIsByteArrayInputStream() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.writeRecord("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link TarBuffer#writeRecord(byte[])} with {@code record}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@code null}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#writeRecord(byte[])}
   */
  @Test
  public void testWriteRecordWithRecord_givenTarBufferWithInStreamIsNull_thenThrowIOException() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer((InputStream) null);

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.writeRecord("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link TarBuffer#flushBlock()}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@link ByteArrayInputStream#ByteArrayInputStream(byte[])}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#flushBlock()}
   */
  @Test
  public void testFlushBlock_givenTarBufferWithInStreamIsByteArrayInputStream() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> (new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")))).flushBlock());
  }

  /**
   * Test {@link TarBuffer#flushBlock()}.
   * <ul>
   *   <li>Given {@link TarBuffer#TarBuffer(InputStream)} with inStream is {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} Debug is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#flushBlock()}
   */
  @Test
  public void testFlushBlock_givenTarBufferWithInStreamIsByteArrayInputStreamDebugIsTrue() throws IOException {
    // Arrange
    TarBuffer tarBuffer = new TarBuffer(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")));
    tarBuffer.setDebug(true);

    // Act and Assert
    assertThrows(IOException.class, () -> tarBuffer.flushBlock());
  }

  /**
   * Test {@link TarBuffer#close()}.
   * <ul>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarBuffer#close()}
   */
  @Test
  public void testClose_thenThrowIOException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> (new TarBuffer(new TarOutputStream(null, 3))).close());
  }
}
