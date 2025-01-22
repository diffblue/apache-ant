package org.apache.tools.bzip2;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import org.apache.tools.bzip2.CBZip2OutputStream.Data;
import org.junit.Test;

public class CBZip2OutputStreamDiffblueTest {
  /**
   * Test Data {@link Data#Data(int)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then return {@link Data#origPtr} is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Data#Data(int)}
   */
  @Test
  public void testDataNewData_whenThree_thenReturnOrigPtrIsZero() {
    // Arrange and Act
    Data actualData = new Data(3);

    // Assert
    assertEquals(0, actualData.origPtr);
    assertEquals(256, actualData.generateMTFValues_yy.length);
    assertEquals(256, actualData.inUse.length);
    assertEquals(256, actualData.unseqToSeq.length);
    assertEquals(260, actualData.heap.length);
    assertEquals(300000, actualData.fmap.length);
    assertEquals(300021, actualData.block.length);
    assertEquals(516, actualData.parent.length);
    assertEquals(516, actualData.weight.length);
    assertEquals(6, actualData.sendMTFValues_code.length);
    assertEquals(6, actualData.sendMTFValues_len.length);
    assertEquals(6, actualData.sendMTFValues_rfreq.length);
    assertEquals(600000, actualData.sfmap.length);
    assertEquals(BZip2Constants.MAX_ALPHA_SIZE, actualData.mtfFreq.length);
    assertEquals(BZip2Constants.MAX_SELECTORS, actualData.selector.length);
    assertEquals(BZip2Constants.MAX_SELECTORS, actualData.selectorMtf.length);
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0}, actualData.sendMTFValues2_pos);
    assertArrayEquals(new int[]{0, 0, 0, 0, 0, 0}, actualData.sendMTFValues_fave);
    assertArrayEquals(new short[]{0, 0, 0, 0, 0, 0}, actualData.sendMTFValues_cost);
    assertArrayEquals(new boolean[]{false, false, false, false, false, false, false, false, false, false, false, false,
        false, false, false, false}, actualData.sentMTFValues4_inUse16);
  }

  /**
   * Test {@link CBZip2OutputStream#hbMakeCodeLengths(char[], int[], int, int)} with {@code len}, {@code freq}, {@code alphaSize}, {@code maxLen}.
   * <p>
   * Method under test: {@link CBZip2OutputStream#hbMakeCodeLengths(char[], int[], int, int)}
   */
  @Test
  public void testHbMakeCodeLengthsWithLenFreqAlphaSizeMaxLen() {
    // Arrange
    char[] len = "A A ".toCharArray();

    // Act
    CBZip2OutputStream.hbMakeCodeLengths(len, new int[]{1, 516, 1, -1}, 3, 3);

    // Assert
    assertArrayEquals("\u0002\u0001\u0002 ".toCharArray(), len);
  }

  /**
   * Test {@link CBZip2OutputStream#hbMakeCodeLengths(char[], int[], int, int)} with {@code len}, {@code freq}, {@code alphaSize}, {@code maxLen}.
   * <p>
   * Method under test: {@link CBZip2OutputStream#hbMakeCodeLengths(char[], int[], int, int)}
   */
  @Test
  public void testHbMakeCodeLengthsWithLenFreqAlphaSizeMaxLen2() {
    // Arrange
    char[] len = "A A ".toCharArray();

    // Act
    CBZip2OutputStream.hbMakeCodeLengths(len, new int[]{1, 1, 1, -1}, 3, 3);

    // Assert
    assertArrayEquals("\u0002\u0002\u0001 ".toCharArray(), len);
  }

  /**
   * Test {@link CBZip2OutputStream#chooseBlockSize(long)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2OutputStream#chooseBlockSize(long)}
   */
  @Test
  public void testChooseBlockSize_whenThree_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1, CBZip2OutputStream.chooseBlockSize(3L));
  }

  /**
   * Test {@link CBZip2OutputStream#chooseBlockSize(long)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then return {@link CBZip2OutputStream#MAX_BLOCKSIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2OutputStream#chooseBlockSize(long)}
   */
  @Test
  public void testChooseBlockSize_whenZero_thenReturnMax_blocksize() {
    // Arrange, Act and Assert
    assertEquals(CBZip2OutputStream.MAX_BLOCKSIZE, CBZip2OutputStream.chooseBlockSize(0L));
  }

  /**
   * Test {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream)}.
   * <p>
   * Method under test: {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream)}
   */
  @Test
  public void testNewCBZip2OutputStream() throws IOException {
    // Arrange
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act and Assert
    assertEquals(CBZip2OutputStream.MAX_BLOCKSIZE, (new CBZip2OutputStream(out)).getBlockSize());
    assertArrayEquals(new byte[]{'h'}, out.toByteArray());
  }

  /**
   * Test {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with {@code 65537}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream)}
   */
  @Test
  public void testNewCBZip2OutputStream_whenByteArrayOutputStreamWith65537() throws IOException {
    // Arrange, Act and Assert
    assertEquals(CBZip2OutputStream.MAX_BLOCKSIZE,
        (new CBZip2OutputStream(new CBZip2OutputStream(new ByteArrayOutputStream(65537), 3))).getBlockSize());
  }

  /**
   * Test {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with {@link CBZip2OutputStream#MAX_BLOCKSIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream)}
   */
  @Test
  public void testNewCBZip2OutputStream_whenByteArrayOutputStreamWithMax_blocksize() throws IOException {
    // Arrange, Act and Assert
    assertEquals(CBZip2OutputStream.MAX_BLOCKSIZE,
        (new CBZip2OutputStream(new CBZip2OutputStream(new ByteArrayOutputStream(CBZip2OutputStream.MAX_BLOCKSIZE), 3)))
            .getBlockSize());
  }

  /**
   * Test {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream)}.
   * <ul>
   *   <li>When {@link PipedOutputStream#PipedOutputStream(PipedInputStream)} with {@link PipedInputStream#PipedInputStream()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream)}
   */
  @Test
  public void testNewCBZip2OutputStream_whenPipedOutputStreamWithPipedInputStream() throws IOException {
    // Arrange, Act and Assert
    assertEquals(CBZip2OutputStream.MAX_BLOCKSIZE,
        (new CBZip2OutputStream(new PipedOutputStream(new PipedInputStream()))).getBlockSize());
  }

  /**
   * Test {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream, int)}.
   * <ul>
   *   <li>When ten.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream, int)}
   */
  @Test
  public void testNewCBZip2OutputStream_whenTen_thenThrowIllegalArgumentException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new CBZip2OutputStream(new ByteArrayOutputStream(1), 10));

  }

  /**
   * Test {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream, int)}.
   * <ul>
   *   <li>When three.</li>
   *   <li>Then return BlockSize is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream, int)}
   */
  @Test
  public void testNewCBZip2OutputStream_whenThree_thenReturnBlockSizeIsThree() throws IOException {
    // Arrange
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act and Assert
    assertEquals(3, (new CBZip2OutputStream(out, 3)).getBlockSize());
    assertArrayEquals(new byte[]{'h'}, out.toByteArray());
  }

  /**
   * Test {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream, int)}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2OutputStream#CBZip2OutputStream(OutputStream, int)}
   */
  @Test
  public void testNewCBZip2OutputStream_whenZero_thenThrowIllegalArgumentException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new CBZip2OutputStream(new ByteArrayOutputStream(1), 0));

  }

  /**
   * Test {@link CBZip2OutputStream#getBlockSize()}.
   * <p>
   * Method under test: {@link CBZip2OutputStream#getBlockSize()}
   */
  @Test
  public void testGetBlockSize() throws IOException {
    // Arrange, Act and Assert
    assertEquals(3, (new CBZip2OutputStream(new ByteArrayOutputStream(1), 3)).getBlockSize());
  }
}
