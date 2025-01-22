package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class FileSystemDiffblueTest {
  /**
   * Test new {@link FileSystem} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FileSystem}
   */
  @Test
  public void testNewFileSystem() {
    // Arrange and Act
    FileSystem actualFileSystem = new FileSystem();

    // Assert
    Location location = actualFileSystem.getLocation();
    assertNull(location.getFileName());
    assertNull(actualFileSystem.getDescription());
    assertNull(actualFileSystem.getProject());
    assertNull(actualFileSystem.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
