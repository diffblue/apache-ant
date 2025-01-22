package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertTrue;
import java.util.Enumeration;
import java.util.StringTokenizer;
import org.junit.Test;

public class EnumerationsDiffblueTest {
  /**
   * Test {@link Enumerations#fromCompound(Enumeration[])}.
   * <p>
   * Method under test: {@link Enumerations#fromCompound(Enumeration[])}
   */
  @Test
  public void testFromCompound() {
    // Arrange and Act
    Enumeration<Object> actualFromCompoundResult = Enumerations.fromCompound(new StringTokenizer("foo"));

    // Assert
    assertTrue(actualFromCompoundResult instanceof CompoundEnumeration);
  }
}
