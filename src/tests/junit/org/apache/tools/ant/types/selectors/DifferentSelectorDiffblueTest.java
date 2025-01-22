package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.junit.Test;

public class DifferentSelectorDiffblueTest {
  /**
   * Test {@link DifferentSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>Given {@link DifferentSelector} (default constructor) addConfigured {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link DifferentSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_givenDifferentSelectorAddConfiguredCutDirsMapper() {
    // Arrange
    DifferentSelector differentSelector = new DifferentSelector();
    differentSelector.addConfigured(new CutDirsMapper());
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        differentSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DifferentSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>Given {@link DifferentSelector} (default constructor) IgnoreContents is {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DifferentSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_givenDifferentSelectorIgnoreContentsIsTrue_thenReturnFalse() {
    // Arrange
    DifferentSelector differentSelector = new DifferentSelector();
    differentSelector.setIgnoreContents(true);
    differentSelector.addConfigured(new CutDirsMapper());
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        differentSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DifferentSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>Given {@link DifferentSelector} (default constructor) IgnoreFileTimes is {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DifferentSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_givenDifferentSelectorIgnoreFileTimesIsFalse_thenReturnTrue() {
    // Arrange
    DifferentSelector differentSelector = new DifferentSelector();
    differentSelector.setIgnoreFileTimes(false);
    differentSelector.addConfigured(new CutDirsMapper());
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        differentSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DifferentSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>Given {@link DifferentSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DifferentSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_givenDifferentSelector_thenReturnTrue() {
    // Arrange
    DifferentSelector differentSelector = new DifferentSelector();
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        differentSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DifferentSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>Given {@link DifferentSelector} (default constructor).</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link DifferentSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_givenDifferentSelector_whenPropertyIsJavaIoTmpdirIsFooToFile() {
    // Arrange
    DifferentSelector differentSelector = new DifferentSelector();
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile();

    // Act and Assert
    assertTrue(
        differentSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DifferentSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link DifferentSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_whenPropertyIsJavaIoTmpdirIs42And42ToFile() {
    // Arrange
    DifferentSelector differentSelector = new DifferentSelector();
    differentSelector.setIgnoreFileTimes(false);
    differentSelector.addConfigured(new CutDirsMapper());
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile();

    // Act and Assert
    assertTrue(
        differentSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DifferentSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} and {@code 42} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link DifferentSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_whenPropertyIsJavaIoTmpdirIsFooAnd42ToFile() {
    // Arrange
    DifferentSelector differentSelector = new DifferentSelector();
    differentSelector.setIgnoreFileTimes(false);
    differentSelector.addConfigured(new CutDirsMapper());
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "foo", "42", "foo").toFile();

    // Act and Assert
    assertTrue(
        differentSelector.selectionTest(srcfile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DifferentSelector#selectionTest(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} and {@code 42} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link DifferentSelector#selectionTest(File, File)}
   */
  @Test
  public void testSelectionTest_whenPropertyIsJavaIoTmpdirIsFooAnd42ToFile2() {
    // Arrange
    DifferentSelector differentSelector = new DifferentSelector();
    differentSelector.setIgnoreFileTimes(false);
    differentSelector.addConfigured(new CutDirsMapper());
    File srcfile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(differentSelector.selectionTest(srcfile,
        Paths.get(System.getProperty("java.io.tmpdir"), "foo", "42", "foo").toFile()));
  }

  /**
   * Test new {@link DifferentSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DifferentSelector}
   */
  @Test
  public void testNewDifferentSelector() {
    // Arrange and Act
    DifferentSelector actualDifferentSelector = new DifferentSelector();

    // Assert
    assertNull(actualDifferentSelector.targetdir);
    Location location = actualDifferentSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDifferentSelector.getDescription());
    assertNull(actualDifferentSelector.getError());
    assertNull(actualDifferentSelector.getProject());
    assertNull(actualDifferentSelector.mapperElement);
    assertNull(actualDifferentSelector.getRefid());
    assertNull(actualDifferentSelector.map);
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(1000, actualDifferentSelector.granularity);
    assertFalse(actualDifferentSelector.isReference());
  }
}
