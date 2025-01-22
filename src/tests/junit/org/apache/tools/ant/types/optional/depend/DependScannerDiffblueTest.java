package org.apache.tools.ant.types.optional.depend;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.junit.Test;

public class DependScannerDiffblueTest {
  /**
   * Test {@link DependScanner#DependScanner(DirectoryScanner)}.
   * <p>
   * Method under test: {@link DependScanner#DependScanner(DirectoryScanner)}
   */
  @Test
  public void testNewDependScanner() {
    // Arrange and Act
    DependScanner actualDependScanner = new DependScanner(new DirectoryScanner());

    // Assert
    assertNull(actualDependScanner.getExcludedDirectories());
    assertNull(actualDependScanner.getExcludedFiles());
    assertNull(actualDependScanner.getNotIncludedDirectories());
    assertNull(actualDependScanner.getNotIncludedFiles());
    assertNull(actualDependScanner.getBasedir());
    assertEquals(0, actualDependScanner.getIncludedDirsCount());
    assertEquals(0, actualDependScanner.getNotFollowedSymlinks().length);
    assertEquals(0, actualDependScanner.getIncludedDirectories().length);
    assertTrue(actualDependScanner.isCaseSensitive());
    assertTrue(actualDependScanner.isEverythingIncluded());
    assertTrue(actualDependScanner.isFollowSymlinks());
  }

  /**
   * Test {@link DependScanner#getIncludedFiles()}.
   * <p>
   * Method under test: {@link DependScanner#getIncludedFiles()}
   */
  @Test
  public void testGetIncludedFiles() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new DependScanner(new DirectoryScanner())).getIncludedFiles());
  }

  /**
   * Test {@link DependScanner#getIncludedFilesCount()}.
   * <p>
   * Method under test: {@link DependScanner#getIncludedFilesCount()}
   */
  @Test
  public void testGetIncludedFilesCount() {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class,
        () -> (new DependScanner(new DirectoryScanner())).getIncludedFilesCount());
  }

  /**
   * Test {@link DependScanner#scan()}.
   * <p>
   * Method under test: {@link DependScanner#scan()}
   */
  @Test
  public void testScan() throws IllegalStateException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new DependScanner(new DirectoryScanner())).scan());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link DependScanner#setCaseSensitive(boolean)}
   *   <li>{@link DependScanner#setExcludes(String[])}
   *   <li>{@link DependScanner#setIncludes(String[])}
   *   <li>{@link DependScanner#setRootClasses(Vector)}
   *   <li>{@link DependScanner#addDefaultExcludes()}
   *   <li>{@link DependScanner#getExcludedDirectories()}
   *   <li>{@link DependScanner#getExcludedFiles()}
   *   <li>{@link DependScanner#getIncludedDirsCount()}
   *   <li>{@link DependScanner#getNotIncludedDirectories()}
   *   <li>{@link DependScanner#getNotIncludedFiles()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    DependScanner dependScanner = new DependScanner(new DirectoryScanner());

    // Act
    dependScanner.setCaseSensitive(true);
    dependScanner.setExcludes(new String[]{"Excludes"});
    dependScanner.setIncludes(new String[]{"Includes"});
    dependScanner.setRootClasses(new Vector<>());
    dependScanner.addDefaultExcludes();
    dependScanner.setCaseSensitive(true);
    dependScanner.setExcludes(new String[]{"Excludes"});
    dependScanner.setIncludes(new String[]{"Includes"});
    String[] actualExcludedDirectories = dependScanner.getExcludedDirectories();
    String[] actualExcludedFiles = dependScanner.getExcludedFiles();
    int actualIncludedDirsCount = dependScanner.getIncludedDirsCount();
    String[] actualNotIncludedDirectories = dependScanner.getNotIncludedDirectories();

    // Assert
    assertNull(actualExcludedDirectories);
    assertNull(actualExcludedFiles);
    assertNull(actualNotIncludedDirectories);
    assertNull(dependScanner.getNotIncludedFiles());
    assertEquals(0, actualIncludedDirsCount);
  }

  /**
   * Test {@link DependScanner#getIncludedDirectories()}.
   * <p>
   * Method under test: {@link DependScanner#getIncludedDirectories()}
   */
  @Test
  public void testGetIncludedDirectories() {
    // Arrange, Act and Assert
    assertEquals(0, (new DependScanner(new DirectoryScanner())).getIncludedDirectories().length);
  }
}
