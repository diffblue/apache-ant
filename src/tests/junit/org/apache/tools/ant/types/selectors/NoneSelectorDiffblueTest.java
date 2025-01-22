package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector;
import org.junit.Test;

public class NoneSelectorDiffblueTest {
  /**
   * Test {@link NoneSelector#toString()}.
   * <ul>
   *   <li>Given {@link NoneSelector} (default constructor) addNone {@link NoneSelector} (default constructor).</li>
   *   <li>Then return {@code {noneselect: }}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#toString()}
   */
  @Test
  public void testToString_givenNoneSelectorAddNoneNoneSelector_thenReturnNoneselect() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addNone(new NoneSelector());

    // Act and Assert
    assertEquals("{noneselect: }", noneSelector.toString());
  }

  /**
   * Test {@link NoneSelector#toString()}.
   * <ul>
   *   <li>Given {@link NoneSelector} (default constructor) addSelector {@link SelectSelector} (default constructor).</li>
   *   <li>Then return {@code {noneselect: }}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#toString()}
   */
  @Test
  public void testToString_givenNoneSelectorAddSelectorSelectSelector_thenReturnNoneselect() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addSelector(new SelectSelector());

    // Act and Assert
    assertEquals("{noneselect: }", noneSelector.toString());
  }

  /**
   * Test {@link NoneSelector#toString()}.
   * <ul>
   *   <li>Given {@link NoneSelector} (default constructor) addSelector {@link SelectSelector} (default constructor).</li>
   *   <li>Then return {@code {noneselect: , }}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#toString()}
   */
  @Test
  public void testToString_givenNoneSelectorAddSelectorSelectSelector_thenReturnNoneselect2() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addSelector(new SelectSelector());
    noneSelector.addSelector(new SelectSelector());

    // Act and Assert
    assertEquals("{noneselect: , }", noneSelector.toString());
  }

  /**
   * Test {@link NoneSelector#toString()}.
   * <ul>
   *   <li>Given {@link NoneSelector} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#toString()}
   */
  @Test
  public void testToString_givenNoneSelector_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new NoneSelector()).toString());
  }

  /**
   * Test {@link NoneSelector#toString()}.
   * <ul>
   *   <li>Given {@link NotSelector#NotSelector()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#toString()}
   */
  @Test
  public void testToString_givenNotSelector_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new NotSelector()).toString());
  }

  /**
   * Test {@link NoneSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {noneselect: {select ScriptSelector}}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#toString()}
   */
  @Test
  public void testToString_thenReturnNoneselectSelectScriptSelector() {
    // Arrange
    SelectSelector selector = new SelectSelector();
    selector.appendSelector(new ScriptSelector());

    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addSelector(selector);

    // Act and Assert
    assertEquals("{noneselect: {select ScriptSelector}}", noneSelector.toString());
  }

  /**
   * Test {@link NoneSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {notselect: {noneselect: ScriptSelector}}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#toString()}
   */
  @Test
  public void testToString_thenReturnNotselectNoneselectScriptSelector() {
    // Arrange, Act and Assert
    assertEquals("{notselect: {noneselect: ScriptSelector}}", (new NotSelector(new ScriptSelector())).toString());
  }

  /**
   * Test {@link NoneSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link NoneSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addModified(new ModifiedSelector());
    noneSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(noneSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link NoneSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link NoneSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile2() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addReadable(new ReadableSelector());
    noneSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(noneSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link NoneSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link NoneSelector} (default constructor) addAnd {@link AndSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenNoneSelectorAddAndAndSelector() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addAnd(new AndSelector());
    noneSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(noneSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link NoneSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link NoneSelector} (default constructor) addNone {@link NoneSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenNoneSelectorAddNoneNoneSelector() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addNone(new NoneSelector());
    noneSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(noneSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link NoneSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link NoneSelector} (default constructor) addSelector {@link SelectSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenNoneSelectorAddSelectorSelectSelector() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    noneSelector.addSelector(new SelectSelector());
    noneSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(noneSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link NoneSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link NoneSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NoneSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenNoneSelector_thenReturnTrue() {
    // Arrange
    NoneSelector noneSelector = new NoneSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(noneSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link NoneSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link NoneSelector}
   */
  @Test
  public void testNewNoneSelector() {
    // Arrange and Act
    NoneSelector actualNoneSelector = new NoneSelector();

    // Assert
    Location location = actualNoneSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualNoneSelector.getDescription());
    assertNull(actualNoneSelector.getError());
    assertNull(actualNoneSelector.getProject());
    assertNull(actualNoneSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualNoneSelector.isReference());
  }
}
