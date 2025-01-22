package org.apache.tools.ant.attribute;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.attribute.IfTrueAttribute.Unless;
import org.junit.Test;

public class IfTrueAttributeDiffblueTest {
  /**
   * Test {@link IfTrueAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link IfTrueAttribute} (default constructor).</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfTrueAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenIfTrueAttribute_when42_thenReturnFalse() {
    // Arrange
    IfTrueAttribute ifTrueAttribute = new IfTrueAttribute();

    // Act and Assert
    assertFalse(ifTrueAttribute.isEnabled(new UnknownElement("Element Name"), "42"));
  }

  /**
   * Test {@link IfTrueAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link IfTrueAttribute} (default constructor).</li>
   *   <li>When {@code on}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfTrueAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenIfTrueAttribute_whenOn_thenReturnTrue() {
    // Arrange
    IfTrueAttribute ifTrueAttribute = new IfTrueAttribute();

    // Act and Assert
    assertTrue(ifTrueAttribute.isEnabled(new UnknownElement("Element Name"), "on"));
  }

  /**
   * Test {@link IfTrueAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link IfTrueAttribute} (default constructor).</li>
   *   <li>When {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfTrueAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenIfTrueAttribute_whenTrueToString_thenReturnTrue() {
    // Arrange
    IfTrueAttribute ifTrueAttribute = new IfTrueAttribute();
    UnknownElement el = new UnknownElement("Element Name");

    // Act and Assert
    assertTrue(ifTrueAttribute.isEnabled(el, Boolean.TRUE.toString()));
  }

  /**
   * Test {@link IfTrueAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link IfTrueAttribute} (default constructor).</li>
   *   <li>When {@code yes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfTrueAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenIfTrueAttribute_whenYes_thenReturnTrue() {
    // Arrange
    IfTrueAttribute ifTrueAttribute = new IfTrueAttribute();

    // Act and Assert
    assertTrue(ifTrueAttribute.isEnabled(new UnknownElement("Element Name"), "yes"));
  }

  /**
   * Test {@link IfTrueAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link Unless} (default constructor).</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfTrueAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenUnless_when42_thenReturnTrue() {
    // Arrange
    Unless unless = new Unless();

    // Act and Assert
    assertTrue(unless.isEnabled(new UnknownElement("Element Name"), "42"));
  }

  /**
   * Test new {@link IfTrueAttribute} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IfTrueAttribute}
   */
  @Test
  public void testNewIfTrueAttribute() {
    // Arrange and Act
    IfTrueAttribute actualIfTrueAttribute = new IfTrueAttribute();

    // Assert
    Location location = actualIfTrueAttribute.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIfTrueAttribute.getDescription());
    assertNull(actualIfTrueAttribute.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualIfTrueAttribute.isPositive());
  }

  /**
   * Test Unless new {@link Unless} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Unless}
   */
  @Test
  public void testUnlessNewUnless() {
    // Arrange and Act
    Unless actualUnless = new Unless();

    // Assert
    Location location = actualUnless.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUnless.getDescription());
    assertNull(actualUnless.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualUnless.isPositive());
  }
}
