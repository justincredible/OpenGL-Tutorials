if ($args.Count -eq 0)
{
	ghc -odir object -hidir object -o main.exe Main.hs
}
else
{
	switch ($args[0].ToLower())
	{
		'clean'
		{
			if (Test-Path main.exe)
			{
				Remove-Item 'main.exe'
			}
			
			Remove-Item 'object/*' -Recurse
		}
	}
}
