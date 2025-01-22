package org.apache.tools.ant.taskdefs.optional.jsp.compilers;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.taskdefs.optional.jsp.Jasper41Mangler;
import org.apache.tools.ant.taskdefs.optional.jsp.JspMangler;
import org.junit.Test;

public class JasperCDiffblueTest {
  /**
   * Test {@link JasperC#JasperC(JspMangler)}.
   * <p>
   * Method under test: {@link JasperC#JasperC(JspMangler)}
   */
  @Test
  public void testNewJasperC() {
    // Arrange, Act and Assert
    assertNull((new JasperC(new Jasper41Mangler())).getJspc());
  }

  /**
   * Test {@link JasperC#createMangler()}.
   * <p>
   * Method under test: {@link JasperC#createMangler()}
   */
  @Test
  public void testCreateMangler() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());

    // Act
    JspMangler actualCreateManglerResult = jasperC.createMangler();

    // Assert
    assertTrue(actualCreateManglerResult instanceof Jasper41Mangler);
    assertNull(actualCreateManglerResult.mapPath("Path"));
    assertSame(jasperC.mangler, actualCreateManglerResult);
  }
}
