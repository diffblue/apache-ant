package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileSet;
import org.junit.Test;

public class BCFileSetDiffblueTest {
  /**
   * Test {@link BCFileSet#BCFileSet()}.
   * <p>
   * Method under test: {@link BCFileSet#BCFileSet()}
   */
  @Test
  public void testNewBCFileSet() {
    // Arrange and Act
    BCFileSet actualBcFileSet = new BCFileSet();

    // Assert
    assertNull(actualBcFileSet.getDir());
    Location location = actualBcFileSet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBcFileSet.getDescription());
    assertNull(actualBcFileSet.getProject());
    assertNull(actualBcFileSet.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, actualBcFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualBcFileSet.isReference());
    assertTrue(actualBcFileSet.getDefaultexcludes());
    assertTrue(actualBcFileSet.getErrorOnMissingDir());
    assertTrue(actualBcFileSet.isFilesystemOnly());
  }

  /**
   * Test {@link BCFileSet#BCFileSet(FileSet)}.
   * <ul>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then return Dir is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BCFileSet#BCFileSet(FileSet)}
   */
  @Test
  public void testNewBCFileSet_whenFileSet_thenReturnDirIsNull() {
    // Arrange and Act
    BCFileSet actualBcFileSet = new BCFileSet(new FileSet());

    // Assert
    assertNull(actualBcFileSet.getDir());
    Location location = actualBcFileSet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBcFileSet.getDescription());
    assertNull(actualBcFileSet.getProject());
    assertNull(actualBcFileSet.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(5, actualBcFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualBcFileSet.isReference());
    assertTrue(actualBcFileSet.getDefaultexcludes());
    assertTrue(actualBcFileSet.getErrorOnMissingDir());
    assertTrue(actualBcFileSet.isFilesystemOnly());
  }

  /**
   * Test {@link BCFileSet#size()}.
   * <ul>
   *   <li>Given {@link BCFileSet#BCFileSet()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BCFileSet#size()}
   */
  @Test
  public void testSize_givenBCFileSetProjectIsProject_thenReturnOne() {
    // Arrange
    BCFileSet bcFileSet = new BCFileSet();
    bcFileSet.setProject(new Project());
    bcFileSet.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(1, bcFileSet.size());
  }
}
