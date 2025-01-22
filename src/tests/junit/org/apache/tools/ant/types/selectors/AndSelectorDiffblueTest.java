package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class AndSelectorDiffblueTest {
  /**
   * Test {@link AndSelector#toString()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) addAnd {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code {andselect: , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#toString()}
   */
  @Test
  public void testToString_givenAndSelectorAddAndAndSelector_thenReturnAndselectScriptSelector() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addAnd(new AndSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: , ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link AndSelector#toString()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) addAnd {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code {andselect: , , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#toString()}
   */
  @Test
  public void testToString_givenAndSelectorAddAndAndSelector_thenReturnAndselectScriptSelector2() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addSelector(new SelectSelector());
    andSelector.addAnd(new AndSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: , , ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link AndSelector#toString()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#toString()}
   */
  @Test
  public void testToString_givenAndSelector_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new AndSelector()).toString());
  }

  /**
   * Test {@link AndSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectScriptSelector() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link AndSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectScriptSelector2() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addSelector(new SelectSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: , ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link AndSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: , , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectScriptSelector3() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addSelector(new SelectSelector());
    andSelector.addSelector(new SelectSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: , , ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link AndSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: {select ScriptSelector}, ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectSelectScriptSelectorScriptSelector() {
    // Arrange
    SelectSelector selector = new SelectSelector();
    selector.appendSelector(new ScriptSelector());

    AndSelector andSelector = new AndSelector();
    andSelector.addSelector(selector);
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: {select ScriptSelector}, ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link AndSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenAndSelector_thenReturnTrue() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(andSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link AndSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AndSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenReturnFalse() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addOr(new OrSelector());
    andSelector.appendSelector(new ScriptSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(andSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link AndSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link AndSelector}
   */
  @Test
  public void testNewAndSelector() {
    // Arrange and Act
    AndSelector actualAndSelector = new AndSelector();

    // Assert
    Location location = actualAndSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAndSelector.getDescription());
    assertNull(actualAndSelector.getError());
    assertNull(actualAndSelector.getProject());
    assertNull(actualAndSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualAndSelector.isReference());
  }
}
