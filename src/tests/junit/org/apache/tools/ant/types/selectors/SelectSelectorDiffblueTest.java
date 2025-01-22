package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector;
import org.junit.Test;

public class SelectSelectorDiffblueTest {
  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) addAnd {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code {select , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_givenSelectSelectorAddAndAndSelector_thenReturnSelectScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addAnd(new AndSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select , ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code {select if: ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_givenSelectSelectorIfIsAndSelector_thenReturnSelectIfScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setIf(new AndSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select if:  ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_givenSelectSelector_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new SelectSelector()).toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select {andselect: ScriptSelector}, ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectAndselectScriptSelectorScriptSelector() {
    // Arrange
    AndSelector selector = new AndSelector();
    selector.appendSelector(new ScriptSelector());

    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addAnd(selector);
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select {andselect: ScriptSelector}, ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select {dateselector date: null compare: equal granularity: 1000}}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectDateselectorDateNullCompareEqualGranularity1000() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addDate(new DateSelector());

    // Act and Assert
    assertEquals("{select {dateselector date: null compare: equal granularity: 1000}}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select if: DifferentSelector ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectIfDifferentSelectorScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setIf(new DifferentSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select if: DifferentSelector ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select if: If Property ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectIfIfPropertyScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setIf((Object) "If Property");
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select if: If Property ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectScriptSelector2() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addSelector(new SelectSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select , ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select ScriptSelector, ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectScriptSelectorScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.appendSelector(new ScriptSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select ScriptSelector, ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select ScriptSelector, , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectScriptSelectorScriptSelector2() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.appendSelector(new ScriptSelector());
    selectSelector.addSelector(new SelectSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select ScriptSelector, , ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select ScriptSelector, , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectScriptSelectorScriptSelector3() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.appendSelector(new ScriptSelector());
    selectSelector.addAnd(new AndSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select ScriptSelector, , ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select unless: DifferentSelector ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectUnlessDifferentSelectorScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setUnless(new DifferentSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select unless: DifferentSelector ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select unless: ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectUnlessScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setUnless(new AndSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select unless:  ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {select unless: Unless Property ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#toString()}
   */
  @Test
  public void testToString_thenReturnSelectUnlessUnlessPropertyScriptSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setUnless((Object) "Unless Property");
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select unless: Unless Property ScriptSelector}", selectSelector.toString());
  }

  /**
   * Test {@link SelectSelector#hasSelectors()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenSelectSelectorAppendSelectorScriptSelector_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertTrue(selectSelector.hasSelectors());
  }

  /**
   * Test {@link SelectSelector#hasSelectors()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenSelectSelector_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new SelectSelector()).hasSelectors());
  }

  /**
   * Test {@link SelectSelector#selectorCount()}.
   * <p>
   * Method under test: {@link SelectSelector#selectorCount()}
   */
  @Test
  public void testSelectorCount() {
    // Arrange, Act and Assert
    assertEquals(0, (new SelectSelector()).selectorCount());
  }

  /**
   * Test {@link SelectSelector#getSelectors(Project)}.
   * <p>
   * Method under test: {@link SelectSelector#getSelectors(Project)}
   */
  @Test
  public void testGetSelectors() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();

    // Act and Assert
    assertEquals(0, selectSelector.getSelectors(new Project()).length);
  }

  /**
   * Test {@link SelectSelector#appendSelector(FileSelector)}.
   * <p>
   * Method under test: {@link SelectSelector#appendSelector(FileSelector)}
   */
  @Test
  public void testAppendSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();

    // Act
    selectSelector.appendSelector(new ScriptSelector());

    // Assert
    assertTrue(selectSelector.hasSelectors());
  }

  /**
   * Test {@link SelectSelector#verifySettings()}.
   * <p>
   * Method under test: {@link SelectSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.appendSelector(new ScriptSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act
    selectSelector.verifySettings();

    // Assert
    assertEquals("Only one selector is allowed within the <selector> tag", selectSelector.getError());
  }

  /**
   * Test {@link SelectSelector#verifySettings()}.
   * <p>
   * Method under test: {@link SelectSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings2() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setError("Only one selector is allowed within the <selector> tag");
    selectSelector.appendSelector(new ScriptSelector());
    selectSelector.appendSelector(new ScriptSelector());

    // Act
    selectSelector.verifySettings();

    // Assert that nothing has changed
    assertEquals("Only one selector is allowed within the <selector> tag", selectSelector.getError());
  }

  /**
   * Test {@link SelectSelector#verifySettings()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor).</li>
   *   <li>Then {@link SelectSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_givenSelectSelector_thenSelectSelectorErrorIsNull() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();

    // Act
    selectSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(selectSelector.getError());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIs42_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) "42");
    selectSelector.setUnless((Object) null);

    // Act and Assert
    assertFalse(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsAntRefid_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) "ant.refid:");
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertFalse(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@link Boolean#FALSE} toString.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsFalseToString_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) Boolean.FALSE.toString());
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertFalse(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code no}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsNo_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) "no");
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertFalse(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsNull_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) null);
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertTrue(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code off}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsOff_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) "off");
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertFalse(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code on}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsOn_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) "on");
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertTrue(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsOne_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf(1);
    selectSelector.setUnless((Object) "");

    // Act and Assert
    assertFalse(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code toString:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsToString_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) "toString:");
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertFalse(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsTrueToString_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) Boolean.TRUE.toString());
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertTrue(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsTrue_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf(true);
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertTrue(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code yes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorIfIsYes_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) "yes");
    selectSelector.setUnless((Object) "42");

    // Act and Assert
    assertTrue(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorProjectIsProject_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(new Project());
    selectSelector.setIf((Object) null);
    selectSelector.setUnless((Object) null);

    // Act and Assert
    assertTrue(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) Unless is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorUnlessIsEmptyString_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) null);
    selectSelector.setUnless((Object) "");

    // Act and Assert
    assertTrue(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) Unless is {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelectorUnlessIsTrue_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) null);
    selectSelector.setUnless(true);

    // Act and Assert
    assertFalse(selectSelector.passesConditions());
  }

  /**
   * Test {@link SelectSelector#passesConditions()}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#passesConditions()}
   */
  @Test
  public void testPassesConditions_givenSelectSelector_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new SelectSelector()).passesConditions());
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addSelector(new SelectSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile2() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addMajority(new MajoritySelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile3() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addModified(new ModifiedSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile4() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addReadable(new ReadableSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) addAnd {@link AndSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSelectSelectorAddAndAndSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addAnd(new AndSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) addOr {@link OrSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSelectSelectorAddOrOrSelector() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.addOr(new OrSelector());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) If is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSelectSelectorIfIs42_thenReturnFalse() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) "42");
    selectSelector.setUnless((Object) null);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSelectSelectorProjectIsProject() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(new Project());
    selectSelector.setIf((Object) null);
    selectSelector.setUnless((Object) null);
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) Unless is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSelectSelectorUnlessIs42() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) null);
    selectSelector.setUnless((Object) "42");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor) Unless is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSelectSelectorUnlessIsEmptyString() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    selectSelector.setProject(null);
    selectSelector.setIf((Object) null);
    selectSelector.setUnless((Object) "");
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SelectSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SelectSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SelectSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSelectSelector_thenReturnTrue() {
    // Arrange
    SelectSelector selectSelector = new SelectSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(selectSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test new {@link SelectSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SelectSelector}
   */
  @Test
  public void testNewSelectSelector() {
    // Arrange and Act
    SelectSelector actualSelectSelector = new SelectSelector();

    // Assert
    Location location = actualSelectSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSelectSelector.getDescription());
    assertNull(actualSelectSelector.getError());
    assertNull(actualSelectSelector.getProject());
    assertNull(actualSelectSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualSelectSelector.isReference());
    assertFalse(actualSelectSelector.hasSelectors());
  }
}
