package org.apache.tools.ant.taskdefs.optional.i18n;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class TranslateDiffblueTest {
  /**
   * Test {@link Translate#execute()}.
   * <ul>
   *   <li>Given {@link Translate} (default constructor) BundleCountry is {@code GB}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Translate#execute()}
   */
  @Test
  public void testExecute_givenTranslateBundleCountryIsGb_thenThrowBuildException() throws BuildException {
    // Arrange
    Translate translate = new Translate();
    translate.setBundleCountry("GB");
    translate.setEndToken("ABC123");
    translate.setStartToken("ABC123");
    translate.setBundle("The bundle attribute must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> translate.execute());
  }

  /**
   * Test {@link Translate#execute()}.
   * <ul>
   *   <li>Given {@link Translate} (default constructor) BundleLanguage is {@code en}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Translate#execute()}
   */
  @Test
  public void testExecute_givenTranslateBundleLanguageIsEn_thenThrowBuildException() throws BuildException {
    // Arrange
    Translate translate = new Translate();
    translate.setBundleLanguage("en");
    translate.setEndToken("ABC123");
    translate.setStartToken("ABC123");
    translate.setBundle("The bundle attribute must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> translate.execute());
  }

  /**
   * Test {@link Translate#execute()}.
   * <ul>
   *   <li>Given {@link Translate} (default constructor) BundleVariant is {@code The todir attribute must be set.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Translate#execute()}
   */
  @Test
  public void testExecute_givenTranslateBundleVariantIsTheTodirAttributeMustBeSet() throws BuildException {
    // Arrange
    Translate translate = new Translate();
    translate.setBundleVariant("The todir attribute must be set.");
    translate.setEndToken("ABC123");
    translate.setStartToken("ABC123");
    translate.setBundle("The bundle attribute must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> translate.execute());
  }

  /**
   * Test {@link Translate#execute()}.
   * <ul>
   *   <li>Given {@link Translate} (default constructor) EndToken is {@code ABC123}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Translate#execute()}
   */
  @Test
  public void testExecute_givenTranslateEndTokenIsAbc123_thenThrowBuildException() throws BuildException {
    // Arrange
    Translate translate = new Translate();
    translate.setEndToken("ABC123");
    translate.setStartToken("ABC123");
    translate.setBundle("The bundle attribute must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> translate.execute());
  }

  /**
   * Test {@link Translate#execute()}.
   * <ul>
   *   <li>Given {@link Translate} (default constructor) StartToken is {@code ABC123}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Translate#execute()}
   */
  @Test
  public void testExecute_givenTranslateStartTokenIsAbc123_thenThrowBuildException() throws BuildException {
    // Arrange
    Translate translate = new Translate();
    translate.setStartToken("ABC123");
    translate.setBundle("The bundle attribute must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> translate.execute());
  }

  /**
   * Test {@link Translate#execute()}.
   * <ul>
   *   <li>Given {@link Translate} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Translate#execute()}
   */
  @Test
  public void testExecute_givenTranslate_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Translate()).execute());
  }

  /**
   * Test {@link Translate#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Translate#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    Translate translate = new Translate();
    translate.setBundle("The bundle attribute must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> translate.execute());
  }

  /**
   * Test new {@link Translate} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Translate}
   */
  @Test
  public void testNewTranslate() {
    // Arrange and Act
    Translate actualTranslate = new Translate();

    // Assert
    Location location = actualTranslate.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTranslate.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualTranslate.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualTranslate.getTaskName());
    assertNull(actualTranslate.getTaskType());
    assertNull(actualTranslate.getProject());
    assertNull(actualTranslate.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualTranslate.hasSelectors());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualTranslate, runtimeConfigurableWrapper.getProxy());
  }
}
