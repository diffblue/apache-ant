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

public class MajoritySelectorDiffblueTest {
  /**
   * Test {@link MajoritySelector#toString()}.
   * <ul>
   *   <li>Given {@link MajoritySelector} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#toString()}
   */
  @Test
  public void testToString_givenMajoritySelector_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new MajoritySelector()).toString());
  }

  /**
   * Test {@link MajoritySelector#toString()}.
   * <ul>
   *   <li>Then return {@code {majorityselect: ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#toString()}
   */
  @Test
  public void testToString_thenReturnMajorityselectScriptSelector() {
    // Arrange
    MajoritySelector majoritySelector = new MajoritySelector();
    majoritySelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{majorityselect: ScriptSelector}", majoritySelector.toString());
  }

  /**
   * Test {@link MajoritySelector#toString()}.
   * <ul>
   *   <li>Then return {@code {majorityselect: , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#toString()}
   */
  @Test
  public void testToString_thenReturnMajorityselectScriptSelector2() {
    // Arrange
    MajoritySelector majoritySelector = new MajoritySelector();
    majoritySelector.addSelector(new SelectSelector());
    majoritySelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{majorityselect: , ScriptSelector}", majoritySelector.toString());
  }

  /**
   * Test {@link MajoritySelector#toString()}.
   * <ul>
   *   <li>Then return {@code {majorityselect: , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#toString()}
   */
  @Test
  public void testToString_thenReturnMajorityselectScriptSelector3() {
    // Arrange
    MajoritySelector majoritySelector = new MajoritySelector();
    majoritySelector.addMajority(new MajoritySelector());
    majoritySelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{majorityselect: , ScriptSelector}", majoritySelector.toString());
  }

  /**
   * Test {@link MajoritySelector#toString()}.
   * <ul>
   *   <li>Then return {@code {majorityselect: , , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#toString()}
   */
  @Test
  public void testToString_thenReturnMajorityselectScriptSelector4() {
    // Arrange
    MajoritySelector majoritySelector = new MajoritySelector();
    majoritySelector.addSelector(new SelectSelector());
    majoritySelector.addSelector(new SelectSelector());
    majoritySelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{majorityselect: , , ScriptSelector}", majoritySelector.toString());
  }

  /**
   * Test {@link MajoritySelector#toString()}.
   * <ul>
   *   <li>Then return {@code {majorityselect: , , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#toString()}
   */
  @Test
  public void testToString_thenReturnMajorityselectScriptSelector5() {
    // Arrange
    MajoritySelector majoritySelector = new MajoritySelector();
    majoritySelector.addSelector(new SelectSelector());
    majoritySelector.addMajority(new MajoritySelector());
    majoritySelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{majorityselect: , , ScriptSelector}", majoritySelector.toString());
  }

  /**
   * Test {@link MajoritySelector#toString()}.
   * <ul>
   *   <li>Then return {@code {majorityselect: {select ScriptSelector}, ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#toString()}
   */
  @Test
  public void testToString_thenReturnMajorityselectSelectScriptSelectorScriptSelector() {
    // Arrange
    SelectSelector selector = new SelectSelector();
    selector.appendSelector(new ScriptSelector());

    MajoritySelector majoritySelector = new MajoritySelector();
    majoritySelector.addSelector(selector);
    majoritySelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{majorityselect: {select ScriptSelector}, ScriptSelector}", majoritySelector.toString());
  }

  /**
   * Test {@link MajoritySelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link MajoritySelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenMajoritySelector_thenReturnTrue() {
    // Arrange
    MajoritySelector majoritySelector = new MajoritySelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(majoritySelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MajoritySelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenReturnFalse() {
    // Arrange
    MajoritySelector majoritySelector = new MajoritySelector();
    majoritySelector.addOr(new OrSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(majoritySelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link MajoritySelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MajoritySelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenReturnTrue() {
    // Arrange
    MajoritySelector majoritySelector = new MajoritySelector();
    majoritySelector.addSelector(new SelectSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(majoritySelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link MajoritySelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MajoritySelector}
   */
  @Test
  public void testNewMajoritySelector() {
    // Arrange and Act
    MajoritySelector actualMajoritySelector = new MajoritySelector();

    // Assert
    Location location = actualMajoritySelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMajoritySelector.getDescription());
    assertNull(actualMajoritySelector.getError());
    assertNull(actualMajoritySelector.getProject());
    assertNull(actualMajoritySelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualMajoritySelector.isReference());
  }
}
