package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class UnparseableExtraFieldDataDiffblueTest {
  /**
   * Test {@link UnparseableExtraFieldData#getLocalFileDataLength()}.
   * <ul>
   *   <li>Then return Value is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getLocalFileDataLength()}
   */
  @Test
  public void testGetLocalFileDataLength_thenReturnValueIsThree() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Act
    ZipShort actualLocalFileDataLength = unparseableExtraFieldData.getLocalFileDataLength();

    // Assert
    assertEquals(3, actualLocalFileDataLength.getValue());
    assertArrayEquals(new byte[]{3, 0}, actualLocalFileDataLength.getBytes());
  }

  /**
   * Test {@link UnparseableExtraFieldData#getLocalFileDataLength()}.
   * <ul>
   *   <li>Then return Value is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getLocalFileDataLength()}
   */
  @Test
  public void testGetLocalFileDataLength_thenReturnValueIsZero() {
    // Arrange and Act
    ZipShort actualLocalFileDataLength = (new UnparseableExtraFieldData()).getLocalFileDataLength();

    // Assert
    assertEquals(0, actualLocalFileDataLength.getValue());
    assertArrayEquals(new byte[]{0, 0}, actualLocalFileDataLength.getBytes());
  }

  /**
   * Test {@link UnparseableExtraFieldData#getCentralDirectoryLength()}.
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Act
    ZipShort actualCentralDirectoryLength = unparseableExtraFieldData.getCentralDirectoryLength();

    // Assert
    assertEquals(3, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{3, 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnparseableExtraFieldData#getCentralDirectoryLength()}.
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength2() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Act
    ZipShort actualCentralDirectoryLength = unparseableExtraFieldData.getCentralDirectoryLength();

    // Assert
    assertEquals(3, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{3, 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnparseableExtraFieldData#getCentralDirectoryLength()}.
   * <ul>
   *   <li>Then return Value is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength_thenReturnValueIsZero() {
    // Arrange and Act
    ZipShort actualCentralDirectoryLength = (new UnparseableExtraFieldData()).getCentralDirectoryLength();

    // Assert
    assertEquals(0, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{0, 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnparseableExtraFieldData#getLocalFileDataData()}.
   * <ul>
   *   <li>Then return {@code AXA} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_thenReturnAxaBytesIsUtf8() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Act
    byte[] actualLocalFileDataData = unparseableExtraFieldData.getLocalFileDataData();

    // Assert
    assertArrayEquals("AXA".getBytes("UTF-8"), actualLocalFileDataData);
  }

  /**
   * Test {@link UnparseableExtraFieldData#getLocalFileDataData()}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new UnparseableExtraFieldData()).getLocalFileDataData());
  }

  /**
   * Test {@link UnparseableExtraFieldData#getCentralDirectoryData()}.
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Act
    byte[] actualCentralDirectoryData = unparseableExtraFieldData.getCentralDirectoryData();

    // Assert
    assertArrayEquals("AXA".getBytes("UTF-8"), actualCentralDirectoryData);
  }

  /**
   * Test {@link UnparseableExtraFieldData#getCentralDirectoryData()}.
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData2() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Act
    byte[] actualCentralDirectoryData = unparseableExtraFieldData.getCentralDirectoryData();

    // Assert
    assertArrayEquals("AXA".getBytes("UTF-8"), actualCentralDirectoryData);
  }

  /**
   * Test {@link UnparseableExtraFieldData#getCentralDirectoryData()}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new UnparseableExtraFieldData()).getCentralDirectoryData());
  }

  /**
   * Test {@link UnparseableExtraFieldData#parseFromLocalFileData(byte[], int, int)}.
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#parseFromLocalFileData(byte[], int, int)}
   */
  @Test
  public void testParseFromLocalFileData() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();

    // Act
    unparseableExtraFieldData.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Assert
    ZipShort centralDirectoryLength = unparseableExtraFieldData.getCentralDirectoryLength();
    assertEquals(3, centralDirectoryLength.getValue());
    byte[] expectedCentralDirectoryData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    assertArrayEquals(new byte[]{3, 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnparseableExtraFieldData#parseFromCentralDirectoryData(byte[], int, int)}.
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#parseFromCentralDirectoryData(byte[], int, int)}
   */
  @Test
  public void testParseFromCentralDirectoryData() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();

    // Act
    unparseableExtraFieldData.parseFromCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Assert
    ZipShort centralDirectoryLength = unparseableExtraFieldData.getCentralDirectoryLength();
    assertEquals(3, centralDirectoryLength.getValue());
    byte[] expectedCentralDirectoryData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    assertArrayEquals(new byte[]{3, 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnparseableExtraFieldData#parseFromCentralDirectoryData(byte[], int, int)}.
   * <p>
   * Method under test: {@link UnparseableExtraFieldData#parseFromCentralDirectoryData(byte[], int, int)}
   */
  @Test
  public void testParseFromCentralDirectoryData2() throws UnsupportedEncodingException {
    // Arrange
    UnparseableExtraFieldData unparseableExtraFieldData = new UnparseableExtraFieldData();
    unparseableExtraFieldData.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Act
    unparseableExtraFieldData.parseFromCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Assert that nothing has changed
    ZipShort centralDirectoryLength = unparseableExtraFieldData.getCentralDirectoryLength();
    assertEquals(3, centralDirectoryLength.getValue());
    byte[] expectedCentralDirectoryData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, unparseableExtraFieldData.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unparseableExtraFieldData.getLocalFileDataData());
    assertArrayEquals(new byte[]{3, 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link UnparseableExtraFieldData}
   *   <li>{@link UnparseableExtraFieldData#getHeaderId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ZipShort actualHeaderId = (new UnparseableExtraFieldData()).getHeaderId();

    // Assert
    assertEquals(44225, actualHeaderId.getValue());
    assertArrayEquals(new byte[]{-63, -84}, actualHeaderId.getBytes());
  }
}
