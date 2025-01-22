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

public class OrSelectorDiffblueTest {
  /**
   * Test {@link OrSelector#toString()}.
   * <ul>
   *   <li>Given {@link OrSelector} (default constructor) addOr {@link OrSelector} (default constructor).</li>
   *   <li>Then return {@code {orselect: , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#toString()}
   */
  @Test
  public void testToString_givenOrSelectorAddOrOrSelector_thenReturnOrselectScriptSelector() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.addOr(new OrSelector());
    orSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{orselect: , ScriptSelector}", orSelector.toString());
  }

  /**
   * Test {@link OrSelector#toString()}.
   * <ul>
   *   <li>Given {@link OrSelector} (default constructor) addOr {@link OrSelector} (default constructor).</li>
   *   <li>Then return {@code {orselect: , , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#toString()}
   */
  @Test
  public void testToString_givenOrSelectorAddOrOrSelector_thenReturnOrselectScriptSelector2() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.addSelector(new SelectSelector());
    orSelector.addOr(new OrSelector());
    orSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{orselect: , , ScriptSelector}", orSelector.toString());
  }

  /**
   * Test {@link OrSelector#toString()}.
   * <ul>
   *   <li>Given {@link OrSelector} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#toString()}
   */
  @Test
  public void testToString_givenOrSelector_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new OrSelector()).toString());
  }

  /**
   * Test {@link OrSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {orselect: ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#toString()}
   */
  @Test
  public void testToString_thenReturnOrselectScriptSelector() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{orselect: ScriptSelector}", orSelector.toString());
  }

  /**
   * Test {@link OrSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {orselect: , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#toString()}
   */
  @Test
  public void testToString_thenReturnOrselectScriptSelector2() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.addSelector(new SelectSelector());
    orSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{orselect: , ScriptSelector}", orSelector.toString());
  }

  /**
   * Test {@link OrSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {orselect: , , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#toString()}
   */
  @Test
  public void testToString_thenReturnOrselectScriptSelector3() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.addSelector(new SelectSelector());
    orSelector.addSelector(new SelectSelector());
    orSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{orselect: , , ScriptSelector}", orSelector.toString());
  }

  /**
   * Test {@link OrSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {orselect: {select ScriptSelector}, ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#toString()}
   */
  @Test
  public void testToString_thenReturnOrselectSelectScriptSelectorScriptSelector() {
    // Arrange
    SelectSelector selector = new SelectSelector();
    selector.appendSelector(new ScriptSelector());

    OrSelector orSelector = new OrSelector();
    orSelector.addSelector(selector);
    orSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{orselect: {select ScriptSelector}, ScriptSelector}", orSelector.toString());
  }

  /**
   * Test {@link OrSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link OrSelector} (default constructor) addAnd {@link AndSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenOrSelectorAddAndAndSelector() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.addAnd(new AndSelector());
    orSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(orSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link OrSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link OrSelector} (default constructor) addModified {@link ModifiedSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenOrSelectorAddModifiedModifiedSelector() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.addModified(new ModifiedSelector());
    orSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(orSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link OrSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link OrSelector} (default constructor) addReadable {@link ReadableSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenOrSelectorAddReadableReadableSelector() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.addReadable(new ReadableSelector());
    orSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(orSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link OrSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link OrSelector} (default constructor) addSelector {@link SelectSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenOrSelectorAddSelectorSelectSelector() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    orSelector.addSelector(new SelectSelector());
    orSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(orSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link OrSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link OrSelector} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link OrSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenOrSelector_thenReturnFalse() {
    // Arrange
    OrSelector orSelector = new OrSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(orSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link OrSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link OrSelector}
   */
  @Test
  public void testNewOrSelector() {
    // Arrange and Act
    OrSelector actualOrSelector = new OrSelector();

    // Assert
    Location location = actualOrSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualOrSelector.getDescription());
    assertNull(actualOrSelector.getError());
    assertNull(actualOrSelector.getProject());
    assertNull(actualOrSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualOrSelector.isReference());
  }
}
