package org.apache.tools.bzip2;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import java.io.UnsupportedEncodingException;
import org.apache.tools.bzip2.CBZip2OutputStream.Data;
import org.junit.Test;

public class BlockSortDiffblueTest {
  /**
   * Test {@link BlockSort#blockSort(Data, int)}.
   * <ul>
   *   <li>Given {@link Data#Data(int)} with blockSize100k is three.</li>
   *   <li>When four.</li>
   *   <li>Then third element is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockSort#blockSort(Data, int)}
   */
  @Test
  public void testBlockSort_givenDataWithBlockSize100kIsThree_whenFour_thenThirdElementIsOne() {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    Data data = new Data(3);

    // Act
    blockSort.blockSort(data, 4);

    // Assert
    int[] intArray = data.fmap;
    assertEquals(1, intArray[2]);
    assertEquals(2, intArray[1]);
    assertEquals(3, intArray[0]);
    assertEquals(3, data.origPtr);
    assertEquals(300000, intArray.length);
    assertEquals(4, intArray[4]);
  }

  /**
   * Test {@link BlockSort#blockSort(Data, int)}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then {@link Data#Data(int)} with blockSize100k is three {@link Data#origPtr} is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockSort#blockSort(Data, int)}
   */
  @Test
  public void testBlockSort_whenMinusOne_thenDataWithBlockSize100kIsThreeOrigPtrIsMinusOne() {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    Data data = new Data(3);

    // Act
    blockSort.blockSort(data, -1);

    // Assert
    assertEquals(-1, data.origPtr);
    int[] intArray = data.fmap;
    assertEquals(0, intArray[0]);
    assertEquals(0, intArray[1]);
    assertEquals(0, intArray[2]);
    assertEquals(0, intArray[4]);
    assertEquals(300000, intArray.length);
  }

  /**
   * Test {@link BlockSort#blockSort(Data, int)}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then {@link Data#Data(int)} with blockSize100k is three {@link Data#origPtr} is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockSort#blockSort(Data, int)}
   */
  @Test
  public void testBlockSort_whenOne_thenDataWithBlockSize100kIsThreeOrigPtrIsZero() {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    Data data = new Data(3);

    // Act
    blockSort.blockSort(data, 1);

    // Assert
    int[] intArray = data.fmap;
    assertEquals(0, intArray[0]);
    assertEquals(0, intArray[2]);
    assertEquals(0, intArray[4]);
    assertEquals(0, data.origPtr);
    assertEquals(1, intArray[1]);
    assertEquals(300000, intArray.length);
  }

  /**
   * Test {@link BlockSort#blockSort(Data, int)}.
   * <ul>
   *   <li>When thirty.</li>
   *   <li>Then {@link Data#Data(int)} with blockSize100k is three {@link Data#origPtr} is twenty-nine.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockSort#blockSort(Data, int)}
   */
  @Test
  public void testBlockSort_whenThirty_thenDataWithBlockSize100kIsThreeOrigPtrIsTwentyNine() {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    Data data = new Data(3);

    // Act
    blockSort.blockSort(data, 30);

    // Assert
    assertEquals(29, data.origPtr);
  }

  /**
   * Test {@link BlockSort#fallbackSort(Data, int)} with {@code data}, {@code last}.
   * <ul>
   *   <li>When four.</li>
   *   <li>Then third element is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockSort#fallbackSort(Data, int)}
   */
  @Test
  public void testFallbackSortWithDataLast_whenFour_thenThirdElementIsOne() {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    Data data = new Data(3);

    // Act
    blockSort.fallbackSort(data, 4);

    // Assert
    int[] intArray = data.fmap;
    assertEquals(1, intArray[2]);
    assertEquals(2, intArray[1]);
    assertEquals(3, intArray[0]);
    assertEquals(300000, intArray.length);
    assertEquals(4, intArray[4]);
  }

  /**
   * Test {@link BlockSort#fallbackSort(Data, int)} with {@code data}, {@code last}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then second element is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockSort#fallbackSort(Data, int)}
   */
  @Test
  public void testFallbackSortWithDataLast_whenMinusOne_thenSecondElementIsZero() {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    Data data = new Data(3);

    // Act
    blockSort.fallbackSort(data, -1);

    // Assert that nothing has changed
    int[] intArray = data.fmap;
    assertEquals(0, intArray[0]);
    assertEquals(0, intArray[1]);
    assertEquals(0, intArray[2]);
    assertEquals(0, intArray[4]);
    assertEquals(300000, intArray.length);
  }

  /**
   * Test {@link BlockSort#fallbackSort(Data, int)} with {@code data}, {@code last}.
   * <ul>
   *   <li>When one.</li>
   *   <li>Then second element is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockSort#fallbackSort(Data, int)}
   */
  @Test
  public void testFallbackSortWithDataLast_whenOne_thenSecondElementIsOne() {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    Data data = new Data(3);

    // Act
    blockSort.fallbackSort(data, 1);

    // Assert
    int[] intArray = data.fmap;
    assertEquals(0, intArray[0]);
    assertEquals(0, intArray[2]);
    assertEquals(0, intArray[4]);
    assertEquals(1, intArray[1]);
    assertEquals(300000, intArray.length);
  }

  /**
   * Test {@link BlockSort#fallbackSort(int[], byte[], int)} with {@code fmap}, {@code block}, {@code nblock}.
   * <p>
   * Method under test: {@link BlockSort#fallbackSort(int[], byte[], int)}
   */
  @Test
  public void testFallbackSortWithFmapBlockNblock() throws UnsupportedEncodingException {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    int[] fmap = new int[]{1, -2097153, 1, -2097153};

    // Act
    blockSort.fallbackSort(fmap, "AXAXAXAX".getBytes("UTF-8"), 1);

    // Assert
    assertArrayEquals(new int[]{0, -2097153, 1, -2097153}, fmap);
  }

  /**
   * Test {@link BlockSort#fallbackSort(int[], byte[], int)} with {@code fmap}, {@code block}, {@code nblock}.
   * <p>
   * Method under test: {@link BlockSort#fallbackSort(int[], byte[], int)}
   */
  @Test
  public void testFallbackSortWithFmapBlockNblock2() throws UnsupportedEncodingException {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    int[] fmap = new int[]{1, -2097153, 1, -2097153};

    // Act
    blockSort.fallbackSort(fmap, "AXAXAXAX".getBytes("UTF-8"), 2);

    // Assert
    assertArrayEquals(new int[]{0, 1, 1, -2097153}, fmap);
  }

  /**
   * Test {@link BlockSort#fallbackSort(int[], byte[], int)} with {@code fmap}, {@code block}, {@code nblock}.
   * <p>
   * Method under test: {@link BlockSort#fallbackSort(int[], byte[], int)}
   */
  @Test
  public void testFallbackSortWithFmapBlockNblock3() throws UnsupportedEncodingException {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    int[] fmap = new int[]{1, -2097153, 1, -2097153};

    // Act
    blockSort.fallbackSort(fmap, "AXAXAXAX".getBytes("UTF-8"), 3);

    // Assert
    assertArrayEquals(new int[]{2, 0, 1, -2097153}, fmap);
  }

  /**
   * Test {@link BlockSort#fallbackSort(int[], byte[], int)} with {@code fmap}, {@code block}, {@code nblock}.
   * <p>
   * Method under test: {@link BlockSort#fallbackSort(int[], byte[], int)}
   */
  @Test
  public void testFallbackSortWithFmapBlockNblock4() {
    // Arrange
    int[] fmap = new int[]{1, -2097153, 1, -2097153};

    // Act
    (new BlockSort(new Data(3))).fallbackSort(fmap, new byte[]{'A', 1, 'A', 'X', 'A', 'X', 'A', 'X'}, 3);

    // Assert
    assertArrayEquals(new int[]{1, 0, 2, -2097153}, fmap);
  }

  /**
   * Test {@link BlockSort#fallbackSort(int[], byte[], int)} with {@code fmap}, {@code block}, {@code nblock}.
   * <ul>
   *   <li>When four.</li>
   * </ul>
   * <p>
   * Method under test: {@link BlockSort#fallbackSort(int[], byte[], int)}
   */
  @Test
  public void testFallbackSortWithFmapBlockNblock_whenFour() throws UnsupportedEncodingException {
    // Arrange
    BlockSort blockSort = new BlockSort(new Data(3));
    int[] fmap = new int[]{1, -2097153, 1, -2097153};

    // Act
    blockSort.fallbackSort(fmap, "AXAXAXAX".getBytes("UTF-8"), 4);

    // Assert
    assertArrayEquals(new int[]{2, 0, 3, 1}, fmap);
  }
}
