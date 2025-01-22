package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class AsiExtraFieldDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link AsiExtraField}
   *   <li>{@link AsiExtraField#setGroupId(int)}
   *   <li>{@link AsiExtraField#setUserId(int)}
   *   <li>{@link AsiExtraField#getGroupId()}
   *   <li>{@link AsiExtraField#getHeaderId()}
   *   <li>{@link AsiExtraField#getLinkedFile()}
   *   <li>{@link AsiExtraField#getMode()}
   *   <li>{@link AsiExtraField#getUserId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    AsiExtraField actualAsiExtraField = new AsiExtraField();
    actualAsiExtraField.setGroupId(1);
    actualAsiExtraField.setUserId(1);
    int actualGroupId = actualAsiExtraField.getGroupId();
    ZipShort actualHeaderId = actualAsiExtraField.getHeaderId();
    String actualLinkedFile = actualAsiExtraField.getLinkedFile();
    int actualMode = actualAsiExtraField.getMode();

    // Assert
    assertEquals("", actualLinkedFile);
    assertEquals(0, actualMode);
    assertEquals(1, actualGroupId);
    assertEquals(1, actualAsiExtraField.getUserId());
    assertEquals(30062, actualHeaderId.getValue());
    assertArrayEquals(new byte[]{'n', 'u'}, actualHeaderId.getBytes());
  }

  /**
   * Test {@link AsiExtraField#getLocalFileDataLength()}.
   * <p>
   * Method under test: {@link AsiExtraField#getLocalFileDataLength()}
   */
  @Test
  public void testGetLocalFileDataLength() {
    // Arrange and Act
    ZipShort actualLocalFileDataLength = (new AsiExtraField()).getLocalFileDataLength();

    // Assert
    assertEquals(14, actualLocalFileDataLength.getValue());
    assertArrayEquals(new byte[]{14, 0}, actualLocalFileDataLength.getBytes());
  }

  /**
   * Test {@link AsiExtraField#getCentralDirectoryLength()}.
   * <p>
   * Method under test: {@link AsiExtraField#getCentralDirectoryLength()}
   */
  @Test
  public void testGetCentralDirectoryLength() {
    // Arrange and Act
    ZipShort actualCentralDirectoryLength = (new AsiExtraField()).getCentralDirectoryLength();

    // Assert
    assertEquals(14, actualCentralDirectoryLength.getValue());
    assertArrayEquals(new byte[]{14, 0}, actualCentralDirectoryLength.getBytes());
  }

  /**
   * Test {@link AsiExtraField#getLocalFileDataData()}.
   * <p>
   * Method under test: {@link AsiExtraField#getLocalFileDataData()}
   */
  @Test
  public void testGetLocalFileDataData() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        (new AsiExtraField()).getLocalFileDataData());
  }

  /**
   * Test {@link AsiExtraField#getCentralDirectoryData()}.
   * <p>
   * Method under test: {@link AsiExtraField#getCentralDirectoryData()}
   */
  @Test
  public void testGetCentralDirectoryData() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        (new AsiExtraField()).getCentralDirectoryData());
  }

  /**
   * Test {@link AsiExtraField#setLinkedFile(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link AsiExtraField} (default constructor) LinkedFile is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link AsiExtraField#setLinkedFile(String)}
   */
  @Test
  public void testSetLinkedFile_whenEmptyString_thenAsiExtraFieldLinkedFileIsEmptyString() {
    // Arrange
    AsiExtraField asiExtraField = new AsiExtraField();

    // Act
    asiExtraField.setLinkedFile("");

    // Assert
    assertEquals("", asiExtraField.getLinkedFile());
    ZipShort centralDirectoryLength = asiExtraField.getCentralDirectoryLength();
    assertEquals(14, centralDirectoryLength.getValue());
    assertFalse(asiExtraField.isLink());
    assertEquals(UnixStat.FILE_FLAG, asiExtraField.getMode());
    assertArrayEquals(new byte[]{14, 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{'9', 'h', -33, -123, 0, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        asiExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'9', 'h', -33, -123, 0, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        asiExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link AsiExtraField#setLinkedFile(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then {@link AsiExtraField} (default constructor) LinkedFile is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AsiExtraField#setLinkedFile(String)}
   */
  @Test
  public void testSetLinkedFile_whenName_thenAsiExtraFieldLinkedFileIsName() {
    // Arrange
    AsiExtraField asiExtraField = new AsiExtraField();

    // Act
    asiExtraField.setLinkedFile("Name");

    // Assert
    assertEquals("Name", asiExtraField.getLinkedFile());
    ZipShort centralDirectoryLength = asiExtraField.getCentralDirectoryLength();
    assertEquals(18, centralDirectoryLength.getValue());
    assertTrue(asiExtraField.isLink());
    assertEquals(UnixStat.LINK_FLAG, asiExtraField.getMode());
    assertArrayEquals(new byte[]{18, 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{'~', '"', -4, 30, 0, -96, 4, 0, 0, 0, 0, 0, 0, 0, 'N', 'a', 'm', 'e'},
        asiExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'~', '"', -4, 30, 0, -96, 4, 0, 0, 0, 0, 0, 0, 0, 'N', 'a', 'm', 'e'},
        asiExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link AsiExtraField#isLink()}.
   * <p>
   * Method under test: {@link AsiExtraField#isLink()}
   */
  @Test
  public void testIsLink() {
    // Arrange, Act and Assert
    assertFalse((new AsiExtraField()).isLink());
  }

  /**
   * Test {@link AsiExtraField#setMode(int)}.
   * <p>
   * Method under test: {@link AsiExtraField#setMode(int)}
   */
  @Test
  public void testSetMode() {
    // Arrange
    AsiExtraField asiExtraField = new AsiExtraField();

    // Act
    asiExtraField.setMode(1);

    // Assert
    assertEquals(32769, asiExtraField.getMode());
    assertArrayEquals(new byte[]{7, 3, 29, 'j', 1, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        asiExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{7, 3, 29, 'j', 1, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        asiExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link AsiExtraField#getMode(int)} with {@code int}.
   * <p>
   * Method under test: {@link AsiExtraField#getMode(int)}
   */
  @Test
  public void testGetModeWithInt() {
    // Arrange, Act and Assert
    assertEquals(32769, (new AsiExtraField()).getMode(1));
  }

  /**
   * Test {@link AsiExtraField#setDirectory(boolean)}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then not {@link AsiExtraField} (default constructor) Directory.</li>
   * </ul>
   * <p>
   * Method under test: {@link AsiExtraField#setDirectory(boolean)}
   */
  @Test
  public void testSetDirectory_whenFalse_thenNotAsiExtraFieldDirectory() {
    // Arrange
    AsiExtraField asiExtraField = new AsiExtraField();

    // Act
    asiExtraField.setDirectory(false);

    // Assert
    assertFalse(asiExtraField.isDirectory());
    assertEquals(UnixStat.FILE_FLAG, asiExtraField.getMode());
    assertArrayEquals(new byte[]{'9', 'h', -33, -123, 0, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        asiExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'9', 'h', -33, -123, 0, Byte.MIN_VALUE, 0, 0, 0, 0, 0, 0, 0, 0},
        asiExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link AsiExtraField#setDirectory(boolean)}.
   * <ul>
   *   <li>When {@code true}.</li>
   *   <li>Then {@link AsiExtraField} (default constructor) Directory.</li>
   * </ul>
   * <p>
   * Method under test: {@link AsiExtraField#setDirectory(boolean)}
   */
  @Test
  public void testSetDirectory_whenTrue_thenAsiExtraFieldDirectory() {
    // Arrange
    AsiExtraField asiExtraField = new AsiExtraField();

    // Act
    asiExtraField.setDirectory(true);

    // Assert
    assertTrue(asiExtraField.isDirectory());
    assertEquals(UnixStat.DIR_FLAG, asiExtraField.getMode());
    assertArrayEquals(new byte[]{'q', 'k', 24, '=', 0, '@', 0, 0, 0, 0, 0, 0, 0, 0},
        asiExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'q', 'k', 24, '=', 0, '@', 0, 0, 0, 0, 0, 0, 0, 0},
        asiExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link AsiExtraField#isDirectory()}.
   * <p>
   * Method under test: {@link AsiExtraField#isDirectory()}
   */
  @Test
  public void testIsDirectory() {
    // Arrange, Act and Assert
    assertFalse((new AsiExtraField()).isDirectory());
  }

  /**
   * Test {@link AsiExtraField#clone()}.
   * <p>
   * Method under test: {@link AsiExtraField#clone()}
   */
  @Test
  public void testClone() {
    // Arrange and Act
    Object actualCloneResult = (new AsiExtraField()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof AsiExtraField);
    assertEquals("", ((AsiExtraField) actualCloneResult).getLinkedFile());
    assertEquals(0, ((AsiExtraField) actualCloneResult).getGroupId());
    assertEquals(0, ((AsiExtraField) actualCloneResult).getMode());
    assertEquals(0, ((AsiExtraField) actualCloneResult).getUserId());
    assertFalse(((AsiExtraField) actualCloneResult).isDirectory());
    assertFalse(((AsiExtraField) actualCloneResult).isLink());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        ((AsiExtraField) actualCloneResult).getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        ((AsiExtraField) actualCloneResult).getLocalFileDataData());
  }
}
