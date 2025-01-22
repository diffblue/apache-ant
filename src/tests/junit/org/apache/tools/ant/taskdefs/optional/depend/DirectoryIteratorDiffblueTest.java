package org.apache.tools.ant.taskdefs.optional.depend;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.junit.Test;

public class DirectoryIteratorDiffblueTest {
  /**
   * Test {@link DirectoryIterator#DirectoryIterator(File, boolean)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryIterator#DirectoryIterator(File, boolean)}
   */
  @Test
  public void testNewDirectoryIterator_whenPropertyIsJavaIoTmpdirIsFooToFile() throws IOException {
    // Arrange and Act
    DirectoryIterator actualDirectoryIterator = new DirectoryIterator(
        Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile(), true);

    // Assert
    assertNull(actualDirectoryIterator.getNextClassFile());
    assertFalse(actualDirectoryIterator.iterator().hasNext());
  }

  /**
   * Test {@link DirectoryIterator#DirectoryIterator(File, boolean)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link DirectoryIterator#DirectoryIterator(File, boolean)}
   */
  @Test
  public void testNewDirectoryIterator_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws IOException {
    // Arrange and Act
    DirectoryIterator actualDirectoryIterator = new DirectoryIterator(
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true);

    // Assert
    assertNull(actualDirectoryIterator.getNextClassFile());
    assertFalse(actualDirectoryIterator.iterator().hasNext());
  }

  /**
   * Test {@link DirectoryIterator#getNextClassFile()}.
   * <p>
   * Method under test: {@link DirectoryIterator#getNextClassFile()}
   */
  @Test
  public void testGetNextClassFile() throws IOException {
    // Arrange
    DirectoryIterator directoryIterator = new DirectoryIterator(
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true);

    // Act and Assert
    assertNull(directoryIterator.getNextClassFile());
    assertFalse(directoryIterator.iterator().hasNext());
  }

  /**
   * Test {@link DirectoryIterator#getNextClassFile()}.
   * <p>
   * Method under test: {@link DirectoryIterator#getNextClassFile()}
   */
  @Test
  public void testGetNextClassFile2() throws IOException {
    // Arrange
    DirectoryIterator directoryIterator = new DirectoryIterator(
        Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile(), true);

    // Act and Assert
    assertNull(directoryIterator.getNextClassFile());
    assertFalse(directoryIterator.iterator().hasNext());
  }
}
