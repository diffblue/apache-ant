package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class UnrecognizedExtraFieldDiffblueTest {
  /**
   * Test {@link UnrecognizedExtraField#setLocalFileDataData(byte[])}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#setLocalFileDataData(byte[])}
   */
  @Test
  public void testSetLocalFileDataData() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();

    // Act
    unrecognizedExtraField.setLocalFileDataData("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    ZipShort centralDirectoryLength = unrecognizedExtraField.getCentralDirectoryLength();
    assertEquals(8, centralDirectoryLength.getValue());
    assertEquals(centralDirectoryLength, unrecognizedExtraField.getLocalFileDataLength());
    byte[] expectedCentralDirectoryData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, unrecognizedExtraField.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unrecognizedExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{'\b', 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnrecognizedExtraField#setLocalFileDataData(byte[])}.
   * <ul>
   *   <li>Then {@link UnrecognizedExtraField} (default constructor) CentralDirectoryData is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnrecognizedExtraField#setLocalFileDataData(byte[])}
   */
  @Test
  public void testSetLocalFileDataData_thenUnrecognizedExtraFieldCentralDirectoryDataIsNull() {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();

    // Act
    unrecognizedExtraField.setLocalFileDataData(null);

    // Assert that nothing has changed
    assertNull(unrecognizedExtraField.getCentralDirectoryData());
    assertNull(unrecognizedExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link UnrecognizedExtraField#getLocalFileDataLength()}.
   * <ul>
   *   <li>Then return Value is eight.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnrecognizedExtraField#getLocalFileDataLength()}
   */
  @Test
  public void testGetLocalFileDataLength_thenReturnValueIsEight() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();
    unrecognizedExtraField.setLocalFileDataData("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ZipShort actualLocalFileDataLength = unrecognizedExtraField.getLocalFileDataLength();

    // Assert
    assertEquals(8, actualLocalFileDataLength.getValue());
    assertArrayEquals(new byte[]{'\b', 0}, actualLocalFileDataLength.getBytes());
  }

  /**
   * Test {@link UnrecognizedExtraField#getLocalFileDataData()}.
   * <ul>
   *   <li>Given {@link UnrecognizedExtraField} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnrecognizedExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_givenUnrecognizedExtraField_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new UnrecognizedExtraField()).getLocalFileDataData());
  }

  /**
   * Test {@link UnrecognizedExtraField#getLocalFileDataData()}.
   * <ul>
   *   <li>Then return {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnrecognizedExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData_thenReturnAxaxaxaxBytesIsUtf8() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();
    unrecognizedExtraField.setLocalFileDataData("AXAXAXAX".getBytes("UTF-8"));

    // Act
    byte[] actualLocalFileDataData = unrecognizedExtraField.getLocalFileDataData();

    // Assert
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), actualLocalFileDataData);
  }

  /**
   * Test {@link UnrecognizedExtraField#setCentralDirectoryData(byte[])}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#setCentralDirectoryData(byte[])}
   */
  @Test
  public void testSetCentralDirectoryData() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();

    // Act
    unrecognizedExtraField.setCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    ZipShort centralDirectoryLength = unrecognizedExtraField.getCentralDirectoryLength();
    assertEquals(8, centralDirectoryLength.getValue());
    byte[] expectedCentralDirectoryData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, unrecognizedExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'\b', 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnrecognizedExtraField#setCentralDirectoryData(byte[])}.
   * <ul>
   *   <li>Then {@link UnrecognizedExtraField} (default constructor) CentralDirectoryData is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnrecognizedExtraField#setCentralDirectoryData(byte[])}
   */
  @Test
  public void testSetCentralDirectoryData_thenUnrecognizedExtraFieldCentralDirectoryDataIsNull() {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();

    // Act
    unrecognizedExtraField.setCentralDirectoryData(null);

    // Assert that nothing has changed
    assertNull(unrecognizedExtraField.getCentralDirectoryData());
  }

  /**
   * Test {@link UnrecognizedExtraField#getCentralDirectoryLength()}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();
    unrecognizedExtraField.setCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ZipShort actualCentralDirectoryLength = unrecognizedExtraField.getCentralDirectoryLength();

    // Assert
    assertEquals(8, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{'\b', 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnrecognizedExtraField#getCentralDirectoryLength()}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength2() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();
    unrecognizedExtraField.setLocalFileDataData("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ZipShort actualCentralDirectoryLength = unrecognizedExtraField.getCentralDirectoryLength();

    // Assert
    assertEquals(8, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{'\b', 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnrecognizedExtraField#getCentralDirectoryData()}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();
    unrecognizedExtraField.setCentralDirectoryData(null);
    unrecognizedExtraField.setLocalFileDataData("AXAXAXAX".getBytes("UTF-8"));

    // Act
    byte[] actualCentralDirectoryData = unrecognizedExtraField.getCentralDirectoryData();

    // Assert
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), actualCentralDirectoryData);
  }

  /**
   * Test {@link UnrecognizedExtraField#getCentralDirectoryData()}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData2() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();
    unrecognizedExtraField.setCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"));
    unrecognizedExtraField.setLocalFileDataData(null);

    // Act
    byte[] actualCentralDirectoryData = unrecognizedExtraField.getCentralDirectoryData();

    // Assert
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), actualCentralDirectoryData);
  }

  /**
   * Test {@link UnrecognizedExtraField#getCentralDirectoryData()}.
   * <ul>
   *   <li>Given {@link UnrecognizedExtraField} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnrecognizedExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData_givenUnrecognizedExtraField_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new UnrecognizedExtraField()).getCentralDirectoryData());
  }

  /**
   * Test {@link UnrecognizedExtraField#parseFromLocalFileData(byte[], int, int)}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#parseFromLocalFileData(byte[], int, int)}
   */
  @Test
  public void testParseFromLocalFileData() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();

    // Act
    unrecognizedExtraField.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Assert
    ZipShort centralDirectoryLength = unrecognizedExtraField.getCentralDirectoryLength();
    assertEquals(3, centralDirectoryLength.getValue());
    assertEquals(centralDirectoryLength, unrecognizedExtraField.getLocalFileDataLength());
    byte[] expectedCentralDirectoryData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, unrecognizedExtraField.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unrecognizedExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{3, 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnrecognizedExtraField#parseFromCentralDirectoryData(byte[], int, int)}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#parseFromCentralDirectoryData(byte[], int, int)}
   */
  @Test
  public void testParseFromCentralDirectoryData() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();

    // Act
    unrecognizedExtraField.parseFromCentralDirectoryData("AXAXAXAX".getBytes("UTF-8"), 2, 3);

    // Assert
    ZipShort centralDirectoryLength = unrecognizedExtraField.getCentralDirectoryLength();
    assertEquals(centralDirectoryLength, unrecognizedExtraField.getLocalFileDataLength());
    byte[] expectedCentralDirectoryData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, unrecognizedExtraField.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unrecognizedExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{3, 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test {@link UnrecognizedExtraField#parseFromCentralDirectoryData(byte[], int, int)}.
   * <p>
   * Method under test: {@link UnrecognizedExtraField#parseFromCentralDirectoryData(byte[], int, int)}
   */
  @Test
  public void testParseFromCentralDirectoryData2() throws UnsupportedEncodingException {
    // Arrange
    UnrecognizedExtraField unrecognizedExtraField = new UnrecognizedExtraField();
    unrecognizedExtraField.setLocalFileDataData("AXAXAXAX".getBytes("UTF-8"));

    // Act
    unrecognizedExtraField.parseFromCentralDirectoryData(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', 'X'}, 2, 3);

    // Assert
    ZipShort centralDirectoryLength = unrecognizedExtraField.getCentralDirectoryLength();
    assertEquals(3, centralDirectoryLength.getValue());
    byte[] expectedCentralDirectoryData = "AXA".getBytes("UTF-8");
    assertArrayEquals(expectedCentralDirectoryData, unrecognizedExtraField.getCentralDirectoryData());
    byte[] expectedLocalFileDataData = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedLocalFileDataData, unrecognizedExtraField.getLocalFileDataData());
    assertArrayEquals(new byte[]{3, 0}, centralDirectoryLength.getBytes());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link UnrecognizedExtraField}
   *   <li>{@link UnrecognizedExtraField#setHeaderId(ZipShort)}
   *   <li>{@link UnrecognizedExtraField#getHeaderId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    UnrecognizedExtraField actualUnrecognizedExtraField = new UnrecognizedExtraField();
    ZipShort headerId = UnicodeCommentExtraField.UCOM_ID;
    actualUnrecognizedExtraField.setHeaderId(headerId);

    // Assert
    assertSame(headerId, actualUnrecognizedExtraField.getHeaderId());
  }
}
