package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.apache.tools.ant.types.Parameter;
import org.junit.Test;

public class BaseExtendSelectorDiffblueTest {
  /**
   * Test {@link BaseExtendSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link ContainsRegexpSelector} (default constructor) Error is {@link ContainsRegexpSelector#EXPRESSION_KEY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseExtendSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenContainsRegexpSelectorErrorIsExpression_key() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();
    containsRegexpSelector.setError(ContainsRegexpSelector.EXPRESSION_KEY);

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertEquals(ContainsRegexpSelector.EXPRESSION_KEY, containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link BaseExtendSelector#setParameters(Parameter[])}.
   * <ul>
   *   <li>Then {@link ContainsRegexpSelector} (default constructor) Error is {@code Invalid parameter Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseExtendSelector#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters_thenContainsRegexpSelectorErrorIsInvalidParameterName() {
    // Arrange
    ContainsRegexpSelector containsRegexpSelector = new ContainsRegexpSelector();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    containsRegexpSelector.setParameters(parameters);

    // Assert
    assertEquals("Invalid parameter Name", containsRegexpSelector.getError());
    assertSame(parameters, containsRegexpSelector.getParameters());
  }

  /**
   * Test {@link BaseExtendSelector#getParameters()}.
   * <p>
   * Method under test: {@link BaseExtendSelector#getParameters()}
   */
  @Test
  public void testGetParameters() {
    // Arrange, Act and Assert
    assertNull((new ContainsRegexpSelector()).getParameters());
  }
}
