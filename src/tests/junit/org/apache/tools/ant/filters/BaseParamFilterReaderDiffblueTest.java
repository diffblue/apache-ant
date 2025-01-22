package org.apache.tools.ant.filters;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.apache.tools.ant.types.Parameter;
import org.junit.Test;

public class BaseParamFilterReaderDiffblueTest {
  /**
   * Test {@link BaseParamFilterReader#setParameters(Parameter[])}.
   * <p>
   * Method under test: {@link BaseParamFilterReader#setParameters(Parameter[])}
   */
  @Test
  public void testSetParameters() {
    // Arrange
    ConcatFilter concatFilter = new ConcatFilter();

    Parameter parameter = new Parameter();
    parameter.setName("Name");
    parameter.setType("Type");
    parameter.setValue("42");
    Parameter[] parameters = new Parameter[]{parameter};

    // Act
    concatFilter.setParameters(parameters);

    // Assert
    assertSame(parameters, concatFilter.getParameters());
  }

  /**
   * Test {@link BaseParamFilterReader#getParameters()}.
   * <p>
   * Method under test: {@link BaseParamFilterReader#getParameters()}
   */
  @Test
  public void testGetParameters() {
    // Arrange, Act and Assert
    assertNull((new ConcatFilter()).getParameters());
  }
}
