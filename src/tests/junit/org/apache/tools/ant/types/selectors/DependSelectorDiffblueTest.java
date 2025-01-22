package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class DependSelectorDiffblueTest {
  /**
   * Test {@link DependSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {dependselector targetdir: NOT YET SET granularity: 1000}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSelector#toString()}
   */
  @Test
  public void testToString_thenReturnDependselectorTargetdirNotYetSetGranularity1000() {
    // Arrange, Act and Assert
    assertEquals("{dependselector targetdir: NOT YET SET granularity: 1000}", (new DependSelector()).toString());
  }

  /**
   * Test {@link DependSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_whenPropertyIsJavaIoTmpdirIs42And42ToFile_thenReturnFalse() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile();

    // Act and Assert
    assertFalse(
        dependSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DependSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_whenPropertyIsJavaIoTmpdirIs42And42ToFile_thenReturnTrue() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(dependSelector.selectionTest(srcfile,
        Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile()));
  }

  /**
   * Test {@link DependSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is empty string toFile.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_whenPropertyIsJavaIoTmpdirIsEmptyStringToFile_thenReturnTrue() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertTrue(
        dependSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DependSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DependSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenReturnFalse() {
    // Arrange
    DependSelector dependSelector = new DependSelector();
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        dependSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link DependSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DependSelector}
   */
  @Test
  public void testNewDependSelector() {
    // Arrange and Act
    DependSelector actualDependSelector = new DependSelector();

    // Assert
    assertNull(actualDependSelector.targetdir);
    Location location = actualDependSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDependSelector.getDescription());
    assertNull(actualDependSelector.getError());
    assertNull(actualDependSelector.getProject());
    assertNull(actualDependSelector.mapperElement);
    assertNull(actualDependSelector.getRefid());
    assertNull(actualDependSelector.map);
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(1000, actualDependSelector.granularity);
    assertFalse(actualDependSelector.isReference());
  }
}
