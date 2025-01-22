package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import java.io.UnsupportedEncodingException;
import java.util.zip.ZipException;
import org.junit.Test;

public class JarMarkerDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link JarMarker}
   *   <li>{@link JarMarker#getInstance()}
   *   <li>{@link JarMarker#getCentralDirectoryData()}
   *   <li>{@link JarMarker#getCentralDirectoryLength()}
   *   <li>{@link JarMarker#getHeaderId()}
   *   <li>{@link JarMarker#getLocalFileDataData()}
   *   <li>{@link JarMarker#getLocalFileDataLength()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    JarMarker actualJarMarker = new JarMarker();
    actualJarMarker.getInstance();
    byte[] actualCentralDirectoryData = actualJarMarker.getCentralDirectoryData();
    ZipShort actualCentralDirectoryLength = actualJarMarker.getCentralDirectoryLength();
    ZipShort actualHeaderId = actualJarMarker.getHeaderId();
    byte[] actualLocalFileDataData = actualJarMarker.getLocalFileDataData();
    ZipShort actualLocalFileDataLength = actualJarMarker.getLocalFileDataLength();

    // Assert
    assertEquals(0, actualCentralDirectoryLength.getValue());
    assertEquals(51966, actualHeaderId.getValue());
    assertSame(actualCentralDirectoryData, actualLocalFileDataData);
    assertSame(actualCentralDirectoryLength, actualLocalFileDataLength);
    assertArrayEquals(new byte[]{}, actualCentralDirectoryData);
    assertArrayEquals(new byte[]{-2, -54}, actualHeaderId.getBytes());
    assertArrayEquals(new byte[]{0, 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link JarMarker#parseFromLocalFileData(byte[], int, int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then throw {@link ZipException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JarMarker#parseFromLocalFileData(byte[], int, int)}
   */
  @Test
  public void testParseFromLocalFileData_whenAxaxaxaxBytesIsUtf8_thenThrowZipException()
      throws UnsupportedEncodingException, ZipException {
    // Arrange
    JarMarker instance = JarMarker.getInstance();

    // Act and Assert
    assertThrows(ZipException.class, () -> instance.parseFromLocalFileData("AXAXAXAX".getBytes("UTF-8"), 2, 3));
  }
}
