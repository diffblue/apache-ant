package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class UnicodePathExtraFieldDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link UnicodePathExtraField#UnicodePathExtraField()}
   *   <li>{@link UnicodePathExtraField#getHeaderId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    UnicodePathExtraField actualUnicodePathExtraField = new UnicodePathExtraField();
    ZipShort actualHeaderId = actualUnicodePathExtraField.getHeaderId();

    // Assert
    assertEquals(0L, actualUnicodePathExtraField.getNameCRC32());
    assertSame(actualUnicodePathExtraField.UPATH_ID, actualHeaderId);
  }

  /**
   * Test {@link UnicodePathExtraField#UnicodePathExtraField(String, byte[])}.
   * <p>
   * Method under test: {@link UnicodePathExtraField#UnicodePathExtraField(String, byte[])}
   */
  @Test
  public void testNewUnicodePathExtraField() throws UnsupportedEncodingException {
    // Arrange and Act
    UnicodePathExtraField actualUnicodePathExtraField = new UnicodePathExtraField("0123456789ABCDEF",
        "AXAXAXAX".getBytes("UTF-8"));

    // Assert
    ZipShort centralDirectoryLength = actualUnicodePathExtraField.getCentralDirectoryLength();
    assertEquals(21, centralDirectoryLength.getValue());
    assertEquals(2442985691L, actualUnicodePathExtraField.getNameCRC32());
    assertEquals(centralDirectoryLength, actualUnicodePathExtraField.getLocalFileDataLength());
    ZipShort expectedHeaderId = actualUnicodePathExtraField.UPATH_ID;
    assertSame(expectedHeaderId, actualUnicodePathExtraField.getHeaderId());
    byte[] expectedUnicodeName = "0123456789ABCDEF".getBytes("UTF-8");
    assertArrayEquals(expectedUnicodeName, actualUnicodePathExtraField.getUnicodeName());
    assertArrayEquals(new byte[]{21, 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{1, -37, 0, -99, -111, '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C',
        'D', 'E', 'F'}, actualUnicodePathExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{1, -37, 0, -99, -111, '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C',
        'D', 'E', 'F'}, actualUnicodePathExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link UnicodePathExtraField#UnicodePathExtraField(String, byte[], int, int)}.
   * <ul>
   *   <li>Then return CentralDirectoryLength Value is twenty-one.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnicodePathExtraField#UnicodePathExtraField(String, byte[], int, int)}
   */
  @Test
  public void testNewUnicodePathExtraField_thenReturnCentralDirectoryLengthValueIsTwentyOne()
      throws UnsupportedEncodingException {
    // Arrange and Act
    UnicodePathExtraField actualUnicodePathExtraField = new UnicodePathExtraField("0123456789ABCDEF",
        "AXAXAXAX".getBytes("UTF-8"), 1, 3);

    // Assert
    ZipShort centralDirectoryLength = actualUnicodePathExtraField.getCentralDirectoryLength();
    assertEquals(21, centralDirectoryLength.getValue());
    assertEquals(289145240L, actualUnicodePathExtraField.getNameCRC32());
    assertEquals(centralDirectoryLength, actualUnicodePathExtraField.getLocalFileDataLength());
    ZipShort expectedHeaderId = actualUnicodePathExtraField.UPATH_ID;
    assertSame(expectedHeaderId, actualUnicodePathExtraField.getHeaderId());
    byte[] expectedUnicodeName = "0123456789ABCDEF".getBytes("UTF-8");
    assertArrayEquals(expectedUnicodeName, actualUnicodePathExtraField.getUnicodeName());
    assertArrayEquals(new byte[]{21, 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(
        new byte[]{1, -104, 1, '<', 17, '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'},
        actualUnicodePathExtraField.getCentralDirectoryData());
    assertArrayEquals(
        new byte[]{1, -104, 1, '<', 17, '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'},
        actualUnicodePathExtraField.getLocalFileDataData());
  }
}
